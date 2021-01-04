library(dplyr)
library(DT)
library(magrittr)
library(readr)
library(rlang)
library(shiny)
library(stringr)
library(tibble)

readTopicsCsv <- function(path) {
    readr::read_csv(path) %>%
        dplyr::transmute(topic, parent, relation) %>%
        dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
        dplyr::distinct() %>%
        dplyr::arrange_all()
}

readResourcesCsv <- function(path) {
    readr::read_csv(path,
                    col_types = readr::cols(
                        topic = readr::col_character(),
                        using = readr::col_character(),
                        audience = readr::col_character(),
                        started = readr::col_logical(),
                        `started at` = readr::col_datetime(),
                        finished = readr::col_logical(),
                        `finished at` = readr::col_datetime(),
                        prerequisites = readr::col_character(),
                        series = readr::col_character()
                    )) %>%
        dplyr::arrange_all()
}

findSubtopics <- function(topics, topic) {
    topics %>%
        dplyr::filter(parent == !!topic & relation == 'subtopic') %>%
        dplyr::pull(topic) %>%
        sort() %>%
        unique()
}

findApplications <- function(topics, topic) {
    topics %>%
        dplyr::filter(parent == !!topic & relation == 'application') %>%
        dplyr::pull(topic) %>%
        sort() %>%
        unique()
}

findRelations <- function(topics, topic) {
    topics %>%
        dplyr::filter(parent == !!topic & relation == 'relation') %>%
        dplyr::pull(topic) %>%
        sort() %>%
        unique()
}

findParentTopics <- function(topics, topic) {
    topics %>%
        dplyr::filter(topic == !!topic) %>%
        dplyr::pull(parent) %>%
        sort() %>%
        unique()
}

getResourceTopics <- function(resources) {
    resources %>%
        dplyr::pull(topic) %>%
        stringr::str_split('\\s*,\\s*') %>%
        unlist() %>%
        sort() %>%
        unique()
}

getUnlistedResourceTopics <- function(topics, resourceTopics) {
    resourceTopics[!(resourceTopics %in% topics$topic)]
}

getRootTopics <- function(topics, unlistedResourceTopics) {
    topics$parent[!(topics$parent %in% topics$topic)] %>%
        `c`(unlistedResourceTopics) %>%
        sort() %>%
        unique() %>%
        {Filter(function(x) nchar(x) > 0, .)}
}

getAllTopics <- function(topics, rootTopics) {
    c(rootTopics, topics$topic) %>%
        unique() %>%
        sort()
}

findResourcesInProgress <- function(resources) {
    resources %>%
        dplyr::mutate(is_finished = (finished | !is.na(`finished at`))) %>%
        dplyr::filter((started | !is.na(`started at`)) & (!is_finished | is.na(is_finished)))
}

getImmediatePrereqs <- function(topics, targetTopics, applicationsOnly = FALSE) {
    targetRelations <- if (isTRUE(applicationsOnly)) {
        'application'
    } else {
        c('subtopic', 'application')
    }
    topics %>%
        dplyr::filter(unlist(lapply(relation, function(relation) relation %in% targetRelations))) %>%
        dplyr::filter(unlist(lapply(topic, function(topic) topic %in% targetTopics))) %>%
        dplyr::pull(parent)
}

findTopicsToLearn <- function(topics, topicsToLearn, applicationsOnly) {
    topicsToLearn <- topicsToLearn
    topicsChecked <- c()
    while (TRUE) {
        topicsToCheck <- setdiff(topicsToLearn, topicsChecked)
        immediatePrereqs <- getImmediatePrereqs(topics, topicsToCheck, applicationsOnly)
        if (length(immediatePrereqs) <= 0) break()
        if (all(immediatePrereqs %in% topicsToLearn)) break()
        topicsChecked <- c(topicsChecked, topicsToCheck)
        topicsToLearn <- c(topicsToLearn, immediatePrereqs)
    }
    topicsToLearn %>% unique()
}

isParentTopic <- function(topics, x, y) {
    (topics %>% dplyr::filter(topic == !!y & parent == !!x) %>% nrow()) > 0 &&
        (topics %>% dplyr::filter(topic == !!x & parent == !!y) %>% nrow()) <= 0
}

getParentTopics <- function(topics, topic) {
    topics %>%
        dplyr::filter(topic == !!topic) %>%
        dplyr::pull(parent) %>%
        unique()
}

# TODO: make sure that we aren't in danger of an infinite loop here (trying to use checked)
isAncestorTopic <- function(topics, x, y, checked = character()) {
    if (isParentTopic(topics, x, y)) TRUE
    else if (isParentTopic(topics, y, x)) FALSE
    else {
        yParents <- getParentTopics(topics, y)
        any(unlist(lapply(
            Filter(function(parent) !(parent %in% checked), yParents),
            function(parent) isAncestorTopic(topics, x, parent,
                                             checked = unique(c(checked, yParents))))))
    }
}
partialRankingToRanking <- function(ranking) {
    max <- max(ranking)
    if (is.na(max)) return(integer())
    tabled <- table(ranking)
    for (num in rev(rlang::seq2(1, max))) {
        if (!(as.character(num) %in% names(tabled))) next()
        # add up all the numbers lower
        offset <- sum(tabled[as.numeric(names(tabled)) < num] - 1)
        newValue <- num + offset
        ranking[ranking == num] <- newValue
        # and then offset num itself if there are multiple of it (e.g., 2, 2, 2 becomes 2, 3, 4)
        if (tabled[[as.character(num)]] > 1) {
            ranking[ranking == newValue] <- rlang::seq2(newValue,
                                                        newValue + tabled[[as.character(num)]] - 1)
        }
    }
    ranking
}

listParentVars <- function(n = 1) {
    frame <- sys.frame(-(n + 1))
    sapply(names(frame), function(name) frame[[name]], simplify = FALSE)
}
getMaxTopicDistance <- function(topics, topic, checked = character(), distances = env()) {
    # TODO: how can we properly detect a loop without messing other things up?
    if (!is.null(distances[[topic]])) return(distances[[topic]])
    parents <- getParentTopics(topics, topic)
    parents <- Filter(function(parent) !(parent %in% checked), parents)
    distance <- if (length(parents) <= 0) 0
    else max(unlist(lapply(parents, function(parent) {
        getMaxTopicDistance(topics, parent, checked = unique(c(checked, topic)),
                            distances = distances)
    }))) + 1
    distances[[topic]] <- distance
    distance
}

sortByRanking <- function(x, ranking) {
    unname(setNames(x, ranking)[as.character(sort(ranking))])
}

findResourcesToLearnFrom <- function(resources, rootTopics, topicsToLearn, showTopicsWithoutResources) {
    numbered <- resources %>%
        dplyr::filter(unlist(lapply(topic, function(resourceTopics) {
            any(topicsToLearn %in% stringr::str_split(resourceTopics, '\\s*,\\s*'))
        }))) %>%
        dplyr::mutate(id = dplyr::row_number())
    resourcesToLearnFrom <- tibble::tibble(topic = topicsToLearn) %>%
        dplyr::mutate(random_resource_id = unlist(lapply(topic, function (targetTopic) {
            filtered <- numbered %>%
                dplyr::filter(unlist(lapply(topic, function(resourceTopics) {
                    targetTopic %in% stringr::str_split(resourceTopics, '\\s*,\\s*')
                })))
            if (nrow(filtered) <= 0) return(NA_integer_)
            filtered %>%
                dplyr::pull(id) %>%
                as.list() %>%
                sample(1)
        }))) %>%
        dplyr::mutate(random_resource = unlist(lapply(random_resource_id, function(targetId) {
            if (length(targetId) <= 0) return(NA_character_)
            result <- numbered %>%
                dplyr::filter(id == targetId) %>%
                dplyr::pull(title)
            if (length(result) <= 0) return(NA_character_)
            result
        }))) %>%
        dplyr::mutate(author = unlist(lapply(random_resource_id, function(targetId) {
            if (length(targetId) <= 0) return(NA_character_)
            result <- numbered %>%
                dplyr::filter(id == targetId) %>%
                dplyr::pull(author)
            if (length(result) <= 0) return(NA_character_)
            result
        }))) %>%
        dplyr::mutate(published = unlist(lapply(random_resource_id, function(targetId) {
            if (length(targetId) <= 0) return(NA_character_)
            result <- numbered %>%
                dplyr::filter(id == targetId) %>%
                dplyr::pull(published)
            if (length(result) <= 0) return(NA_character_)
            result
        }))) %>%
        dplyr::mutate(link = unlist(lapply(random_resource_id, function(targetId) {
            if (length(targetId) <= 0) return(NA_character_)
            result <- numbered %>%
                dplyr::filter(id == targetId) %>%
                dplyr::pull(link)
            if (length(result) <= 0) return(NA_character_)
            result
        }))) %>%
        {table <- .; if (!showTopicsWithoutResources) {
            table %>% dplyr::filter(!is.na(random_resource))
        } else {
            table
        }} %>%
        dplyr::mutate(is_root_topic = topic %in% rootTopics)
}

buildLearningPlan <- function(resources, topics, rootTopics, topicsToLearn,
                              applicationsOnly = FALSE, showTopicsWithoutResources = FALSE) {
    topicsToLearn <- findTopicsToLearn(topics, topicsToLearn, applicationsOnly)
    distances <- env()
    # alternate way of sorting: give each topic a number
    # representing its maximum distance from the root
    # and then sort based on that function (this can be the ranking)
    maxTopicDistances <- unlist(lapply(topicsToLearn, function(topic) {
        getMaxTopicDistance(topics, topic, distances = distances)
    }))
    # We add 1 because they start at 0
    ranking <- partialRankingToRanking(maxTopicDistances + 1)
    topicsToLearn <- sortByRanking(topicsToLearn, ranking)
    resourcesToLearnFrom <- findResourcesToLearnFrom(resources, rootTopics, topicsToLearn,
                                                     showTopicsWithoutResources)
    resourcesToLearnFrom %>%
        dplyr::transmute(topic, `random resource title` = random_resource, author,
                         published, link, `root topic` = is_root_topic)
}

ui <- shiny::navbarPage(
    'Resource DB',
    tabPanel(
        'Learn',
        shiny::uiOutput('learn')
    ),
    tabPanel(
        'Browse',
        shiny::uiOutput('browse')
    ),
    tabPanel(
        'Topics',
        DT::DTOutput('topics')
    ),
    tabPanel(
        'Resources',
        DT::DTOutput('resources')
    ),
    tabPanel(
        'In progress',
        DT::DTOutput('inProgress')
    ),
    tabPanel(
        'Finished',
        DT::DTOutput('finished')
    ),
    tabPanel(
        'Own',
        DT::DTOutput('own')
    ),
    tabPanel(
        'Own hard copy',
        DT::DTOutput('ownHardCopy')
    ),
    tabPanel(
        'On loan',
        DT::DTOutput('onLoan')
    ),
    tabPanel(
        'Unlisted Resource Topics',
        DT::DTOutput('unlistedResourceTopics')
    ),
    tabPanel(
        'Root Topics',
        DT::DTOutput('rootTopics')
    )
)

server <- function(input, output) {
    topics <- readTopicsCsv('../../../notes/resources/topics.csv')
    resources <- readResourcesCsv('../../../notes/resources/resources.csv')
    
    currentTopicReactive <- shiny::reactiveVal(NULL)
    
    resourceTopics <- getResourceTopics(resources)
    
    unlistedResourceTopics <- getUnlistedResourceTopics(topics, resourceTopics)
    
    rootTopics <- getRootTopics(topics, unlistedResourceTopics)
    
    allTopics <- getAllTopics(topics, rootTopics)
    
    parentTopicsReactive <- shiny::reactive({
        currentTopic <- currentTopicReactive()
        if (!is.null(currentTopic)) {
            findParentTopics(topics, currentTopic)
        }
    })
    
    subtopicsReactive <- shiny::reactive({
        currentTopic <- currentTopicReactive()
        if (is.null(currentTopic)) {
            rootTopics
        } else {
            findSubtopics(topics, currentTopic)
        }
    })
    
    applicationsReactive <- shiny::reactive({
        currentTopic <- currentTopicReactive()
        if (is.null(currentTopic)) {
            NULL
        } else {
            findApplications(topics, currentTopic)
        }
    })
    
    relationsReactive <- shiny::reactive({
        currentTopic <- currentTopicReactive()
        if (is.null(currentTopic)) {
            NULL
        } else {
            findRelations(topics, currentTopic)
        }
    })
    
    output$browse <- shiny::renderUI({
        currentTopic <- currentTopicReactive()
        parentTopics <- parentTopicsReactive()
        parentTopicsCount <- length(parentTopics)
        subtopics <- subtopicsReactive()
        applications <- applicationsReactive()
        relations <- relationsReactive()
        # TODO: also attempt to show all the descendant topics
        # in a table below the relevant resources?
        # TODO: show the parent, grandparent, great-grandparent, etc. topics all in one select?
        relevantResources <- resources %>%
            dplyr::filter(unlist(lapply(topic, function(topic) {
                any(currentTopic %in% (topic %>% stringr::str_split('\\s*,\\s*')))
            })))
        shiny::div(
            if (!is.null(currentTopic)) {
                shiny::p(
                    shiny::actionButton('goBackToTop', 'Back to top')
                )
            },
            if (!is.null(currentTopic)) {
                shiny::h3(currentTopic)
            },
            if (parentTopicsCount > 0) {
                shiny::p(
                    shiny::selectInput('parentTopic', 'Choose parent topic', parentTopics),
                    shiny::actionButton('chooseParentTopic', 'Go to parent topic')
                )
            },
            if (length(subtopics)) {
                shiny::p(
                    shiny::selectInput("subtopic", "Choose subtopic", subtopics, selectize = TRUE),
                    shiny::actionButton('chooseSubtopic', 'Go to subtopic')
                )
            },
            if (length(applications)) {
                shiny::p(
                    shiny::selectInput("application", "Choose application", applications),
                    shiny::actionButton('chooseApplication', 'Go to application')
                )
            },
            if (length(relations)) {
                shiny::p(
                    shiny::selectInput("relation", "Choose relation", applications),
                    shiny::actionButton('chooseRelation', 'Go to relation')
                )
            },
            if (nrow(relevantResources) > 0) {
                DT::renderDT({
                    relevantResources
                })
            }
        )
    })
    
    shiny::observeEvent(input$chooseParentTopic, {
        currentTopicReactive(input$parentTopic)
    })
    
    shiny::observeEvent(input$chooseSubtopic, {
        currentTopicReactive(input$subtopic)
    })
    
    shiny::observeEvent(input$chooseApplication, {
        currentTopicReactive(input$application)
    })
    
    shiny::observeEvent(input$goBackToTop, {
        currentTopicReactive(NULL)
    })
    
    output$resources <- DT::renderDT({
        resources
    })
    
    output$inProgress <- DT::renderDT({
        findResourcesInProgress(resources)
    })
    
    output$finished <- DT::renderDT({
        resources %>%
            dplyr::filter(finished | !is.na(`finished at`))
    })
    
    output$own <- DT::renderDT({
        resources %>%
            dplyr::filter(!is.na(ownership))
    })
    
    output$onLoan <- DT::renderDT({
        resources %>%
            dplyr::filter(stringr::str_detect(ownership, '^(borrowed|on loan|on rental|rented)'))
    })
    
    output$ownHardCopy <- DT::renderDT({
        resources %>%
            dplyr::filter(stringr::str_detect(ownership, 'hard copy|hardcover|paperback'))
    })
    
    output$topics <- DT::renderDT({
        topics
    })
    
    output$unlistedResourceTopics <- DT::renderDT({
        tibble::tibble(topic = unlistedResourceTopics)
    })
    
    output$rootTopics <- DT::renderDT({
        tibble::tibble(topic = rootTopics)
    })
    
    # TODO: fix sorting of output to put earlier concepts first
    # (maybe do a sort comparing whether one is a parent of the other)
    # TODO: instead of random, prefer things that are owned or freely available
    # TODO: add option to prefer more recent books
    output$learningPlan <- DT::renderDT({
        topicToLearn <- input$topicToLearn
        # TODO: fix applications only to also show applications of topics
        # that were parent topics of the target topic
        # might need to do that by changing it to filter at the end instead
        applicationsOnly <- input$applicationsOnly
        showTopicsWithoutResources <- input$showTopicsWithoutResources
        if (is.null(topicToLearn) || nchar(topicToLearn) <= 0) {
            return()
        }
        buildLearningPlan(resources, topics, rootTopics, topicToLearn,
                          applicationsOnly, showTopicsWithoutResources)
    })
    
    output$learn <- shiny::renderUI({
        shiny::div(
            shiny::selectInput('topicToLearn',
                               'Choose topic to learn',
                               unique(sort(allTopics)),
                               selectize = TRUE),
            shiny::checkboxInput('showTopicsWithoutResources', 'Show topics without resources'),
            shiny::checkboxInput('applicationsOnly', 'Use applications only?'),
            shiny::h3('Learning Plan'),
            DT::DTOutput('learningPlan')
        )
    })
}

shinyApp(ui = ui, server = server)
