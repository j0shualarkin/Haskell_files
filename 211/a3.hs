module ComposingFunctions where

data Month = "January"
  | "February"
  | "March"
  | "April"
  | "May"
  | "June"
  | "July"
  | "August"
  | "September"
  | "October"
  | "November"
  | "December"

-- Exercise 1

next_month :: Month -> Month
-- returns the next month
next_month "January" = "February"
next_month "February" = "March"
next_month "March" = "April"
next_month "April" = "May"
next_month "May" = "June"
next_month "June" = "July"
next_month "July" = "August"
next_month "August" = "September"
next_month "September" = "October"
next_month "October" = "November"
next_month "November" = "December"
next_month "December" = "January"
