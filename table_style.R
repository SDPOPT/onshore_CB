library(DT)
library(formattable)


RG_bar <- function(x) {

style1 <- function(x) {
  style1 <- style( display = "inline-block", 
                   "border-radius" = "4px", 
                   "padding-right" = "2px", 
                   "background-color" = csscolor("green"),
                   width = percent(normalize(abs(x))))
return(style1)
}

style2 <- function(x) {
style2 <- style( display = "inline-block", 
                 "border-radius" = "4px", 
                 "padding-right" = "2px", 
                 "background-color" = csscolor("red"),
                 width = percent(normalize(abs(x))))
return(style2)
}

format <- formatter("span", style = x ~ ifelse(x >= 0, style1(x), style2(x)))

return(format)
}
