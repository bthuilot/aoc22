module Day00 (day00) where

import Interface (DayPart)

day00 ::[DayPart]
day00 = [
  const $ return "hello, world!",
  const $ return "HELLO, WORLD!"
  ]
