# Introduction to Shiny apps in R
This workshop is designed as an overview of *some* of the tools for writing Shiny apps. [Shiny](https://shiny.rstudio.com) is an R package for writing web applications around your code and data. If you've never heard of Shiny before, have a quick look at [this](https://shiny.rstudio.com/gallery/) excellent gallery of example web apps that have been curated by Rstudio.

# What you'll need for this workshop
Here are the packages that we'll use in this workshop:
```R
# The absolute minimum
install.packages("shiny")
install.packages("ggplot")
install.packages("plotly")
install.packages("dplyr")

# Package for profiling code
install.packages("profvis")

# Packages for making your application look different
install.packages("shinydashboard")
install.packages("bslib")
```
# The general structure of this workshop
If you are familiar with writing R code, then you probably won't have much trouble converting it to work with user selectable inputs. In fact, that's how this workshop starts! We'll look at an R notebook that makes cool example visualizations with a toy dataset. Then we'll look at what it takes to convert that R notebook into a functioning Shiny app.

At first we focused mostly on just converting our R code into something that works with Shiny. We got something that works, but maybe there are more efficient ways to structure our code to run better in the Shiny context. Let's find out by profiling our code with [profvis](https://rstudio.github.io/profvis/index.html).

Next, we'll start thinking about [reactivity](https://shiny.rstudio.com/articles/reactivity-overview.html), which is the underlying logic that controls how Shiny works. 

After reactivity, we'll go over customizing the look of your Shiny app with packages like [flexdashboard](https://pkgs.rstudio.com/flexdashboard/) and [bslib](https://rstudio.github.io/bslib/index.html). If there's time, we'll go through writing your own CSS to customize the look of your app.

Finally, we'll cover a few options for deploying your Shiny app to the web. 

# The dataset
We will be using Health Canada's nutrient dataset to look at nutrient values for a variety of ingredients. This workshop is based off of [yihanwu's excellent Shiny app](https://github.com/yihanwu/Nutrient_Calculator).

I picked this dataset because the amount of stuff to explore in it is immense. There are over 5000 food items to select, and for each of them we can look at the associated nutrient information. There are many ways you could choose to visualize the nutrition data. For instance, we could look at the raw nutritional value vs the percent of the daily recommended value. We could plot the information or put it in a table. We could even combine food items and estimate their total nutritional content. 

---

# Where to go from here
I suspect that's all we'll have time to get to. That won't stop me from thinking about some more advanced topics to potentially cover in the future! Here's what I'm considering:

- Dynamic UIs
- Conditional UIs
- Performant code for heavy computation/simultaneous users
  - Application programming interfaces (APIs) with [Plumber](https://www.rplumber.io)
  - Asynchronous programming with [Future](https://rstudio.github.io/promises/articles/futures.html) and [Promises](https://rstudio.github.io/promises/)
- Structuring Shiny deployments with Docker

If you'd like to suggest topics to integrate into this workshop, you can email me or file it as an issue with this repository.


