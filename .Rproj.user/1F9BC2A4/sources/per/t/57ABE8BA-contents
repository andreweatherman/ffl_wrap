#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(gt)
library(gtExtras)
library(tidyverse)
library(ffscrapr)

# get starters
# starters <- ff_starters(con) |> select(-eligible_lineup_slots)
starters <- read.csv('starters.csv')

# get player names
player_names <- starters |> 
  arrange(player_name) |> 
  filter(!grepl('D/ST', player_name)) |> 
  pluck(9) |> 
  unique()

# get draft
draft <- read.csv('draft.csv')

# get scoring history
scoring_hist <- read.csv('scoring_hist.csv')

# define franchise tibble
franchise_tibble <- tibble(
  team = ff_franchises(con) |> pluck(2),
  name = c('Sam Whitestone', 'Phillip Jennings', 'Allie Whitestone', 
           'Tom Whitestone', 'JJ DeVos', 'Nolan DeVos', 'Dan DeVos', 'Andrew Weatherman')
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy Football Wrapped!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition = 'input.tabselected==1',
            sliderInput("players",
                        "Number of Players:",
                        min = 1,
                        max = 50,
                        value = 10),
            sliderInput("points",
                        "Minimum total points:",
                        min = 50,
                        max = 250,
                        value = 100),
            pickerInput('teams',
                        label = 'Filter teams:',
                        choices = c('Allie', 'Andrew', 'Dan', 'JJ', 
                                    'Nolan', 'Phillip', 'Sam', 'Tom'),
                        multiple = TRUE,
                        selected = c('Allie', 'Andrew', 'Dan', 'JJ', 
                                     'Nolan', 'Phillip', 'Sam', 'Tom'),
                        options = list(title = 'Select one or more owners',
                                       `selected-text-format` = 'count > 3',
                                       `actions-box` = TRUE)
            ),
            prettyToggle('include_qb',
                         label_on = 'With QBs',
                         label_off = 'No QBs',
                         value = FALSE,
                         shape = 'curve',
                         animation = 'rotate')
            
        ),
        conditionalPanel(condition = 'input.tabselected==2',
                         sliderInput('round_selected',
                                     value = 1,
                                     min = 1,
                                     max = 16,
                                     step = 1,
                                     label = 'Round Progress:'),
                         pickerInput('team_selection',
                                     label = 'Select owner:',
                                     multiple = TRUE,
                                     choices = franchise_tibble |> arrange(name) |> pluck(2),
                                     choicesOpt = list(
                                       subtext = franchise_tibble |> arrange(name) |> pluck(1)
                                     ))),
        conditionalPanel(condition = 'input.tabselected==3',
                         pickerInput(
                           'player_name',
                           label = 'Filter player:',
                           choices = player_names,
                           selected = 'Jalen Hurts',
                           multiple = FALSE,
                           options = list(
                             `live-search` = TRUE,
                             title = 'Select one player',
                             size = 10
                           )
                         ),
                         prettyRadioButtons(
                           'show_stats',
                           label = 'Choose stats to show:',
                           choices = c('Passing', 'Rushing', 'Receiving', 'Rush + Catch'),
                           selected = 'Passing',
                           icon = icon('check'),
                           status = 'success',
                           animation = 'jelly'
                         )
                         )),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = 'tabs',
                      id = 'tabselected',
           tabPanel('Overperforming Players', gt_output("overperform"), value = 1),
           tabPanel('Draft Recap', gt_output('draft'), value = 2),
           tabPanel('Player Scoring Recap', gt_output('player_recap'), value=3)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # set tokens
  swid = '41913F6A-5AEA-48D4-913F-6A5AEA58D4F3'
  espn_s2 = 'AEBNVkIqYaOd5Oqobre2EZDQjvo%2BOWweECu8IZ0IVZ7Mqn%2FiglAX%2B8gkrdzUV6LfJbKGD7EYWLrCqMbH%2FtBJW%2FIv1kJ7CCEHC0IQKTGL9oE0%2FgXd5TrX12hvciAa75TKybe26B2Mpi1TC8NOtsBfGXqwa4%2FIyFWx0zoqxthGRf5506e7x%2FH5%2BYNbZf5j3UGTmmNrH%2FGi3lu8HN6GU93k%2FR5n9%2B1haw2fEPMAeRdd0kmrL5Thloc4I86jfS6hJmzRsJbvKQOUFsOq2IlIZApcSGFFSGp5iqIgCRh0XN9BM1swsw%3D%3D'
  

  # establish connection
  con <- espn_connect(
    season = 2022,
    league_id = 425862554,
    espn_s2 = espn_s2,
    swid = swid
  )
  
  # define vector for looking up team name
  franchise <- ff_franchises(con) |> pluck(2)
  names(franchise) <- c('Sam', 'Phillip', 'Allie', 'Tom', 'JJ', 'Nolan', 'Dan', 'Andrew')
  
  # filter for round selected in draft 
  draft_data <- reactive(
      draft |> 
        filter(round == input$round_selected)
)
  # get pos. rank for indv. weeks
  pos_rank_week <- scoring_hist |> 
    group_by(pos, week) |> 
    arrange(desc(points)) |> 
    mutate(pos_rank = row_number(),
           espn_id = as.character(espn_id)) |> 
    select(espn_id, pos_rank) |> 
    ungroup()
  
  # get team player was on for each week
  player_team <- starters |> 
    select(week, franchise_name, player_id) |> 
    mutate(player_id = as.character(player_id))
  
  # get if player started in a given week
  did_start <- starters |>
    mutate(start = ifelse(lineup_slot == 'BE', '', 'check'),
           player_id = as.character(player_id)) |> 
    select(week, player_id, start)
  
  # filter for selected player in recap
  player_recap <- reactive(
    scoring_hist |> 
      filter(player_name == input$player_name) |> 
      mutate(fumbles = sack_fumbles_lost + receiving_fumbles_lost + rushing_fumbles_lost,
             two_pt_scores = receiving_2pt_conversions + rushing_2pt_conversions + passing_2pt_conversions,
             espn_id = as.character(espn_id)) |> 
      left_join(pos_rank_week |> select(week, espn_id, pos_rank), by = c('espn_id', 'week')) |> 
      left_join(player_team, by = c('week', 'espn_id' = 'player_id')) |> 
      left_join(did_start, by = c('week', 'espn_id' = 'player_id')) |> 
      mutate(franchise_name = ifelse(is.na(franchise_name), 'FREE AGENT', franchise_name))
  )
  
  # get points for players and pos. rank
  pos_rank <- starters |> 
    group_by(player_id, pos) |> 
    summarize(points = sum(player_score), .groups = 'drop') |> 
    group_by(pos) |> 
    arrange(desc(points)) |> 
    mutate(pos_rank = row_number()) |> 
    left_join(starters |> 
                select(player_name, player_id) |> 
                unique(),
              by = 'player_id')
  
  # get current team for starters
  current_team <- starters |> 
    group_by(player_name) |> 
    slice_tail(n = 1) |> 
    select(player_name, franchise_name) |> 
    ungroup()
  
  # filter for selected teams in overerforming
  selected_teams <- reactive({
    selected_teams <- c()
    for (i in 1:length(input$teams)) {
      selected_teams <- append(selected_teams, franchise[input$teams[i]])
    }
    selected_teams <- unname(selected_teams)
  }
  )
  
  # filter for selected owner in draft recap
  draft_recap_owner <- reactive(
    draft |> 
      left_join(franchise_tibble |> rename('owner' = 'name'), by = c('franchise_name' = 'team')) |> 
      filter(owner == input$team_selection)
  )
  
  # filter for selected stats in player recap
  player_recap_data <- reactive({
    stat <- switch(input$show_stats,
           'Passing' = 'passing',
           'Rushing' = 'rushing',
           'Receiving' = 'catch',
           `Rush + Catch` = 'flex')
    if(stat == 'passing') {
    player_recap <- player_recap() |> 
      select('week', 'player_name', 'pos', 'points', 'passing_yards', 'passing_tds', 'interceptions',
             'two_pt_scores', 'fumbles', 'pos_rank', 'franchise_name', 'start') |> 
      setNames(c('week', 'player', 'pos', 'points', 'yards', 'tds', 'int', '2PT', 'fumbles', 'rank', 'team', 'start'))
    }
    
    else if(stat == 'rushing') {
      player_recap() |> 
        select('week', 'player_name', 'pos', 'points', 'rushing_yards', 'rushing_tds',  
               'two_pt_scores', 'fumbles', 'pos_rank', 'franchise_name', 'start') |> 
        setNames(c('week', 'player', 'pos', 'points', 'Yards', 'TDs', '2PT', 'Fumbles', 'rank', 'team', 'start'))
    }
    
    else if (stat == 'catch') {
      player_recap() |> 
        select('week', 'player_name', 'pos', 'points', 'receptions', 'receiving_yards', 'receiving_tds',
               'two_pt_scores', 'fumbles', 'pos_rank', 'franchise_name', 'start') |> 
        setNames(c('week', 'player', 'pos', 'points', 'Rec.', 'Yards', 'TDs', '2PT', 'Fumbles', 'rank', 'team', 'start'))
    }
    
    else if (stat == 'flex') {
      player_recap() |>
        select('week', 'player_name', 'pos', 'points', 'rushing_yards', 'rushing_tds', 
               'receptions', 'receiving_yards', 'receiving_tds', 'two_pt_scores', 'fumbles', 'pos_rank',
               'franchise_name', 'start') |> 
        setNames(c('week', 'player', 'pos', 'points', 'Rush Yards', 'Rush TDs', 'Catches', 
                   'Rec. Yards', 'Rec. TDs', '2PT', 'Fumbles', 'rank', 'team', 'start'))
    }
      
  })
  
  # filter for qb
  is_qb <- reactive({
    is_qb <- ifelse(input$include_qb == FALSE, 'QB', '')
  })
  
  # overperforming players =======
  data <- 
    reactive(
      starters |> 
      filter(lineup_slot != 'BE') |> 
      group_by(player_name, pos)  |> 
      summarize(tot_score = sum(player_score), 
                proj_score = sum(projected_score, na.rm = TRUE),
                proj_diff = tot_score - proj_score,
                per_game = tot_score / n(), 
                games = n(),
                .groups = 'drop') |> 
      left_join(current_team, by = 'player_name') |> 
      mutate(stacked = paste0(pos, ' | ', franchise_name)) |> 
      arrange(desc(proj_diff)) |> 
      filter(tot_score >= input$points & 
             franchise_name %in% selected_teams() &
             !pos %in% is_qb()) |>
      slice(1:input$players) |> 
      select(-c(pos, franchise_name)) |> 
      setNames(c('Player', 'Total', 'Projected', 'Difference', 'PPG', 'Games', 'Stack'))
    )
  
    output$overperform <- render_gt({
      data() |> 
      gt() |> 
        gt_theme_nytimes() |> 
        gt_color_rows(Difference, palette = 'ggsci::blue_material') |> 
        cols_align(align = 'center') |> 
        fmt_number(Difference, decimals = 1) |> 
        fmt_number(PPG, decimals = 1) |> 
        gt_merge_stack(Player, Stack, small_cap = FALSE) |> 
        tab_header(title = 'Top Fantasy Overperformers',
                   subtitle = 'Actual points vs. expected for starting fantasy players in Last Dance Fantasy') |> 
        tab_source_note('Entering Week 16 (Round Two)') |> 
        tab_options(source_notes.font.size = 12) |> 
        opt_row_striping()
    })
    
    output$draft <- render_gt({
        draft_data() |> 
          select(player_id, round, pick, overall, franchise_name, player_name, pos, autodraft_type) |> 
          left_join(franchise_tibble, by = c('franchise_name' = 'team')) |>
          left_join(pos_rank |> ungroup() |> select(player_id, pos_rank), by  = 'player_id') |> 
          select(-player_id) |> 
          gt() |> 
          gt_theme_nytimes() |> 
          gt_merge_stack(franchise_name, name, small_cap = FALSE) |> 
          gt_color_rows(pos_rank, palette = 'ggsci::green_material', direction = -1) |> 
          cols_label(pos_rank = 'Final Rank') |> 
          tab_style(
            style = cell_text(weight = 'bold'),
            locations = cells_body(
              columns = player_name,
              rows = autodraft_type == 3
            )
          ) |> 
          cols_hide(autodraft_type) |> 
          tab_header(title = 'Last Dance Fantasy Draft Recap',
                     subtitle = 'Draft summary for the 2022 Last Dance Fantasy draft') |> 
          tab_source_note('Bolded players denote an autodrafted pick') |> 
          tab_options(source_notes.font.size = 12)
      })
    
    output$player_recap <- render_gt({
      player_recap_data() |> 
        mutate(stack = paste0(pos, ' | ', team)) |> 
        select(-c(pos, team)) |> 
        arrange(week) |> 
        gt() |> 
        gt_theme_nytimes() |> 
        gt_merge_stack(player, stack, small_cap = FALSE) |> 
        gt_fa_column(start) |> 
        tab_style(
          style = cell_text(weight = 'bold'),
          locations = cells_body(
            columns = points
          )
        ) |> 
        gt_color_rows(rank, palette = 'ggsci::green_material', 
                      direction = -1, domain = c(1, 30)) |> 
        cols_align(align = 'center') |> 
        fmt_number(points, decimals = 1) |> 
        tab_header(title = paste0('Player Scoring Recap: ', input$player_name),
                   subtitle = 'Scoring recap on Last Dance Fantasy league scoring rules')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
