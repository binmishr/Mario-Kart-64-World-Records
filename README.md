# Mario-Kart-64-World-Records

The details of the codeset and plots are included in the attached Microsoft Word Document (.docx) file in this repository. 
You need to view the file in "Read Mode" to see the contents properly after downloading the same.

    library(tidyverse)
    library(ggtext)
    library(ggridges)

    # importing data

    tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

    drivers <- tuesdata$drivers
    records <- tuesdata$records

    # activity plot

    records |>
      separate(date, into = c("year", "month", "day"), sep = "-") |>
      filter(shortcut == "No") |>
      ggplot(aes(x = year, y = track, group = track)) +
      geom_density_ridges(rel_min_height = 0.01, fill = "#F97B64", alpha = .8) +
      labs(x = "", y = "", title = "Mario Kart 64 World Record Activity per Track",
           subtitle = "Plenty of records set near release, with a slump in activity followed by a surge in the 2010s") +
      scale_x_discrete(breaks = c(1997, 2009, 2021)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, .12))) +
      theme(legend.position = "none",
            axis.text.x = element_text(face = "bold", colour = "#3A5678"),
            axis.text.y = element_text(vjust = 0.3, face = "bold", colour = "#3A5678"),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            panel.grid.major.y = element_line(size = 0.3, colour = "black", linetype = 3),
            plot.title.position = "plot",
            plot.title = element_text(face = "bold", colour = "#AE030E", size = 18),
            plot.background = element_rect(fill = "#FFF8E7"),
            panel.background = element_rect(fill = "#FFF8E7"),
            plot.margin = margin(20,20,10,20))

    # calculating the total times per year

    all <- records |>
      separate(date, into = c("year", "month", "day"), sep = "-") |>
      filter(type == "Three Lap") |>
      expand(year, track, shortcut)

    fastest <- records |>
      separate(date, into = c("year", "month", "day"), sep = "-") |>
      filter(type == "Three Lap") |>
      group_by(year, track, shortcut) |>
      summarise(min = min(time)) |>
      ungroup()

    total_times <- all |>
      left_join(fastest) |>
      pivot_wider(names_from = c("track", "shortcut"), values_from = "min") |>
      fill(where(is.double), .direction = "down") |>
      pivot_longer(-1) |>
      fill(where(is.double), .direction = "down") |>
      separate(col = "name", into = c("track", "shortcut"), sep = "_") |>
      group_by(year, shortcut) |>
      summarise(total_time = sum(value)) |>
      ungroup() |>
      mutate(shortcut = factor(shortcut))

    # total times plot

    total_times |>
      pivot_wider(names_from = "shortcut", values_from = "total_time") %>% 
      ggplot(aes(x = year)) +
      geom_linerange(aes(ymin = Yes/60, ymax = No/60), colour = "#4C4A42", linetype = 3) +
      geom_point(aes(y = No/60), size = 3, colour = "#E03135") +
      geom_point(aes(y = Yes/60), size = 3, colour = "#185AA6") +
      labs(x = "", y = "Minutes",
           title = "Total time to finish all Mario Kart 64 tracks<br>at world record pace",
           subtitle = "<span style = 'color:#E03135;'>Without</span> shortcuts vs. <span style = 'color:#185AA6;'>with</span> shortcuts",
           caption = "World record pace calculated as lowest time per track each year") +
      expand_limits(x = c(-1, 28), y = c(15, 45)) +
      scale_x_discrete(breaks = seq(1997, 2021, by = 3)) +
      theme(legend.position = "none",
            axis.text.x = element_text(face = "bold", colour = "#4D556B"),
            axis.text.y = element_text(face = "bold", colour = "#4D556B"),
            axis.title.y = element_text(face = "bold", colour = "#4D556B"),
            axis.line = element_line(colour = "#4D556B"),
            panel.grid = element_blank(),
            plot.title = element_textbox(face = "bold", colour = "#AE030E", size = 18),
            plot.background = element_rect(fill = "#FFF8E7"),
            panel.background = element_rect(fill = "#FFF8E7"),
            plot.margin = margin(20,20,20,20),
            plot.subtitle = element_textbox(face = "bold"),
            plot.caption = element_textbox(face = "italic", colour = "#4D556B"))

    The plots are given on the attached .docx file in this repository
