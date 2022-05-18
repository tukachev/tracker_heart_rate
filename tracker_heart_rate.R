library(tidyverse)
library(lubridate)
library(glue)
library(showtext)
library(ggtext)

font_add_google("Oswald", family = "Oswald")
showtext_auto()

tracker_heart_rate_full <-
  read_csv(
    "com.samsung.shealth.tracker.heart_rate.202205152109.csv",
    skip = 1
  )

tracker_heart_rate <- tracker_heart_rate_full %>%
  select(
    com.samsung.health.heart_rate.end_time,
    com.samsung.health.heart_rate.heart_rate
  ) %>%
  rename(date_time = com.samsung.health.heart_rate.end_time,
         heart_rate = com.samsung.health.heart_rate.heart_rate) %>%
  mutate_at("date_time", format, tz = "Asia/Yekaterinburg") %>%
  mutate_at("date_time", as_datetime) %>%
  mutate(
    date = floor_date(date_time, "day") %>% as_date,
    month = month(date_time, label = TRUE, abbr = FALSE),
    year = year(date_time),
    weekday = weekdays(date, abbreviate = TRUE),
    hour = hour(date_time)
  ) %>%
  mutate(weekday = factor(weekday,
                          levels = rev(c(
                            "Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс"
                          ))))

data_points <- tracker_heart_rate %>%
  group_by(year, month) %>%
  count()

data_points_year <- tracker_heart_rate %>%
  group_by(year) %>%
  summarise(
    n = n(),
    month_min = min(month),
    month_max = max(month),
    mean_heart_rate = mean(heart_rate)
  ) %>%
  mutate(period = glue("<span style = 'color:black;'>{year}</span> {month_min} − {month_max} | <span style = 'color:black;'>{n}</span> измерений"))

year_lebels <- c(
    `2020` = data_points_year$period[1],
    `2021` = data_points_year$period[2],
    `2022` = data_points_year$period[3]
  )

tracker_heart_rate <- tracker_heart_rate %>%
  mutate(year_text = recode(year, `2020` = data_points_year$period[1],
                       `2021` = data_points_year$period[2],
                       `2022` = data_points_year$period[3]))

tracker_heart_rate %>%
  group_by(year_text, weekday, hour) %>%
  summarize(
    heat_rate = round(mean(heart_rate), 1),
    count = n()) %>%
  ggplot(aes(x = hour, weekday, fill = heat_rate)) +
  geom_tile(color = "white", width = 0.98, height = 0.98) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 24, 1), expand = c(0, 0)) +
    scale_fill_gradient("Среднее значение ЧСС", low = "yellow", high = "red") +
    labs(x = "Время дня", y = "", 
    caption = "Данные: SamsungHealth\nВизуализация: Юрий Тукачев, май 2022") +
    ggtitle(label = "Изменение частоты сердечных сокращений (ЧСС)",
            subtitle = glue(
                  "Мой средний пульс по часам дня недели | годам наблюдений (всего {nrow(tracker_heart_rate)} измерений)"
                )
              ) +
  guides(
    fill = guide_colorbar(
    ticks.linewidth = 1.8,
    title.position = 'top',
    title.hjust = .5,
    barwidth = unit(15, 'lines'),
    label.position = "top",
    barheight = unit(1, 'lines'))
        ) +
  facet_wrap( ~ year_text, ncol = 1) +
  theme(
      plot.title.position = "plot",
      text = element_text(size = 26, family = "Oswald"),
      plot.caption.position = "plot",
      plot.caption = element_text(color = "gray"),
      plot.subtitle = element_text(size = 24, color = "gray40"),
      legend.position = "top",
      panel.background = element_blank(),
      plot.margin = margin(25, 25, 10, 25),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      axis.text.y.left = element_text(hjust = 0),
      strip.text = element_markdown(size = 22, color = "gray40", hjust =  0),
      legend.text = element_text(size = 18, color = "gray40"),
      legend.title = element_text(size = 18, color = "gray40"))

ggsave("heart_rate.png", dpi = 120, height = 8.8, width = 8, scale = 1.15)
            