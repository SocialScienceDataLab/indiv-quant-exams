## ---- Teaching Quantitative Social Science in Times of COVID-19 --------------
## ---- How to Generate and Distribute Individualized Exams with R and RMarkdown
## 
## This is a collection of R Code presented in the corresponding tutorial
## on Methods Bites - Blog of the MZES Social Science Data Lab
## URL: mzes.uni-mannheim.de/socialsciencedatalab/article/indiv-quant-exams


## ----Code: R packages used in this tutorial-----------------------------------
## ---- CRAN Packages ----
## Save package names as a vector of strings
pkgs <-  c("rmarkdown",
           "bookdown",
           "knitr",
           "stargazer",
           "dplyr",
           "xtable")

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())],
       install.packages,
       dependencies = TRUE)

## Load all packages to library
lapply(pkgs,
       library,
       character.only = TRUE)

## ---- Other Packages ----
remotes::install_github("datawookie/emayili")
library(emayili)


## ----Output: Sample student roster--------------------------------------------
student_roster <- data.frame(
  first_name = c("Denis", "Marcel", "Oliver"),
  last_name = c("Cohen", "Neunhoeffer", "Rittmann"),
  login_id = c("dcohen", "mneunhoe", "orittman"),
  takes_exam = c(FALSE, TRUE, TRUE)
)
student_roster


## ----Code: Setup code chunk of RMD template-----------------------------------
## ---- Setup ----
## Save package names as a vector of strings
pkgs <-  c("bookdown",
           "knitr",
           "stargazer",
           "dplyr",
           "xtable",
           "MASS")

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())],
       install.packages,
       dependencies = TRUE)

## Load all packages to library
lapply(pkgs,
       library,
       character.only = TRUE)


## Global chunk options
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)
options(scipen=999)

## Functions
inRange <- function(x, a, b) {
  (x - a)  *  (b - x) >= 0
}


## ----Code: Randomizing numerical inputs for calculations----------------------
## ---- Seed ----
set.seed(PLACEHOLDER_SEED)

## ---- Regression example ----
## Generate numerics
N <- 1000L
mu_X <- sample(45:55, 1)
sd_X <- sample(10:15, 1)
pi_W <- sample(seq(.2, .8, .1), 1)
mu_Z <- sample(9:12, 1)
sd_Z <- sample(2:4, 1)
beta <- c(-sample(12:8, 1),
          sample(seq(0.5, 1.5, 0.5), 1),
          -sample(7:3, 1),
          sample(1:5, 1))
se <- abs(beta) * c(4, 10, 4, 6)^(-1)

## Descriptives table
descriptives <- 
  rbind(
    c("Age", mu_X, sd_X),
    c("Female", pi_W, "(dummy)"),
    c("Education", mu_Z, sd_Z)
  ) 
colnames(descriptives) <- 
  c("Variable", "Mean", "Std. Dev.")

## Regression table
regression_table <- cbind(format(beta, nsmall = 1, digits = 0),
                          format(se, nsmall = 1, digits = 1))
rownames(regression_table) <-
  names(beta) <- names(se) <- c("Intercept",
                                "Age",
                                "Female",
                                "Education")
colnames(regression_table) <-
  c("Beta", "Std. Err.")


## ----Code: Randomize question prompt------------------------------------------
## Randomize
set.seed(PLACEHOLDER_SEED)
cat_select <- sample(1:4, 1)

## Define scenarios for introductoty text
cat_intro <- c(
  paste0(
    "Suppose you have sampled four voters from Vermont. ",
    "Each voter has a 2/3 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Idaho. ",
    "Each voter has a 1/3 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Wyoming. ",
    "Each voter has a 3/10 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Washington DC.",
    " Each voter has a 9/10 probability of voting for Biden."
  )
)

cat_prob <- c(2 / 3,
              1 / 3,
              3 / 10,
              9 / 10)

## Print introductory text for prompt
cat(cat_intro[cat_select])
cat('\n')


## ----Code: Randomize number of Biden voters-----------------------------------
## Randomize
set.seed(PLACEHOLDER_SEED)
cat_select <- sample(1:4, 1)

## Define scenarios for introductoty text
cat_intro <- c(
  paste0(
    "Suppose you have sampled four voters from Vermont. ",
    "Each voter has a 2/3 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Idaho. ",
    "Each voter has a 1/3 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Wyoming. ",
    "Each voter has a 3/10 probability of voting for Biden."
  ),
  paste0(
    "Suppose you have sampled four voters from Washington DC.",
    " Each voter has a 9/10 probability of voting for Biden."
  )
)

cat_prob <- c(2 / 3,
              1 / 3,
              3 / 10,
              9 / 10)

## Print introductory text for prompt
cat(cat_intro[cat_select])
cat('\n')


## ----Code: RMD code for individualized solutions------------------------------
## Randomize
set.seed(PLACEHOLDER_SEED)
k_select <- sample(1:3, 1)

## Print questions
cat(paste0("1. What is the probability that exactly ", k_select, " out of 4 voters vote for Biden?"))
cat('\n')
cat(paste0("2. What is the expected value for the number of Biden voters in the sample?"))


## ----Code: Defining a nested list for items and alternatives------------------
## ---- Questions ----
single_choice <- list()
single_choice[[1]] <- list()
single_choice[[1]]$Q <-
  "When we use the term '95% confidence interval' for a sample mean, we mean that..."
single_choice[[1]]$A <- list(
  "we can reject the null hypothesis with 95 percent confidence.",
  "there is a 95 percent probability that the population mean is within the interval.",
  "the true population parameter is contained in the interval about 95 times if we were to repeat the experiment one hundred times.",
  "any given 95 percent confidence interval from a random sample will contain the true population mean.",
  "None of the other answers."
)
single_choice[[1]]$C <- "the true population parameter is contained in the interval about 95 times if we were to repeat the experiment one hundred times."

single_choice[[2]] <- list()
single_choice[[2]]$Q <-
  "Let $X_1$ and $X_2$ be two different values of an independent variable. A first-difference is..."
single_choice[[2]]$A <- list(
  "a quantity of interest from a regression model defined as $E(Y|X_1)/E(Y|X_2)$.",
  "a quantity of interest  from a regression model defined as $Y|X_1 - Y|X_2$.",
  "a quantity of interest  from a regression model defined as $E(Y|X_1)+E(Y|X_2)$.",
  "a quantity of interest  from a regression model defined as $E(Y|1)-E(Y|0)$.",
  "None of the other answers."
)
single_choice[[2]]$C <- "None of the other answers."


## ----Code: Randomizing and printing items and choice alternatives-------------
## ---- Randomize ----
set.seed(PLACEHOLDER_SEED)
q_order <- sample(seq_along(single_choice), length(single_choice))
a_order <-
  lapply(single_choice, function(q)
    sample(seq_along(q$A), length(q$A)))

## ---- Print ----
for (q in q_order) {
  cat('\n')
  cat(paste0("1. ", single_choice[[q]]$Q))
  cat('\n')
  for (a in a_order[[q]]) {
    cat(paste0("    a. ", single_choice[[q]]$A[[a]]))
    cat('\n')
  }
}


## ----Code: Printing individualized single-choice solutions--------------------
## ---- Print ----
for (q in q_order) {
  cat('\n')
  cat(paste0("1. ",
             letters[which(unlist(single_choice[[q]]$A[a_order[[q]]]) ==
                             single_choice[[q]]$C)],
             "."))
  cat('\n')
}


## ----Code: Generating individualized exams (setup)----------------------------
## ---- Setup ----
## Save package names as a vector of strings
pkgs <-  c("dplyr",
           "rmarkdown")

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())],
       install.packages,
       dependencies = TRUE)

## Load all packages to library
lapply(pkgs,
       library,
       character.only = TRUE)

## Function
letter2number <- function(x) {
  (utf8ToInt(x) - utf8ToInt("a") + 1L) %>%
    paste0(collapse = "") %>%
    as.numeric()
}


## ----R Code: Generating individualized exams (step 1--------------------------
## ---- Students list and seeds ----
student_roster <- student_roster %>%
  dplyr::filter(takes_exam) %>%
  group_by(login_id) %>%
  dplyr::mutate(seed = letter2number(login_id)) %>%
  dplyr::mutate(seed = as.integer(substr(seed, 1, 8)))


## ----R Code: Generating individualized exams (step 2)-------------------------
## ---- Generate exams and solutions from template ----
## Read in templates
template_exam <- readChar(
  "02-midterm-exam/midterm-exam-template.Rmd",
  file.info("02-midterm-exam/midterm-exam-template.Rmd")$size
)
template_solution <- readChar(
  "02-midterm-exam/midterm-solutions-template.Rmd",
  file.info("02-midterm-exam/midterm-solutions-template.Rmd")$size
)


for (s in seq_len(nrow(students))) {
  ## Generate and save individualized exams
  tmp_script <- template_exam
  tmp_script <- gsub("PLACEHOLDER_SEED",
                     as.numeric(students[s, "seed"]),
                     tmp_script)
  cat(tmp_script, file = paste0("02-midterm-exam/",
                                "individualized-exams-rmd/",
                                "midterm_",
                                as.character(students[s, "Login"]),
                                ".Rmd"))

  ## Generate and save individualized solutions
  tmp_script <- template_solution
  tmp_script <- gsub("PLACEHOLDER_SEED",
                     as.numeric(students[s, "seed"]),
                     tmp_script)
  cat(tmp_script, file = paste0("02-midterm-exam/",
                                "individualized-solutions-rmd/",
                                "solutions_",
                                as.character(students[s, "Login"]),
                                ".Rmd"))
}


## ----R Code: Generating individualized exams (step 3)-------------------------
## ---- Render exams and solutions-----
## Exams
rmd_files <- list.files("02-midterm-exam/individualized-exams-rmd")
rmd_files <- rmd_files[grepl(".Rmd", rmd_files)]
for (rmd_file in rmd_files) {
  rmarkdown::render(
    paste0("02-midterm-exam/individualized-exams-rmd/", rmd_file),
    output_dir = "02-midterm-exam/individualized-exams-pdf")
}

## Solutions
rmd_files <- list.files("02-midterm-exam/individualized-solutions-rmd")
rmd_files <- rmd_files[grepl(".Rmd", rmd_files)]
for (rmd_file in rmd_files) {
  rmarkdown::render(
    paste0("02-midterm-exam/individualized-solutions-rmd/", rmd_file),
    output_dir = "02-midterm-exam/individualized-solutions-pdf")
}



## ----R Code: Distributing individualized exams (step 1)-----------------------
## List file names of individualized exam prompts
exam_path <- "../02-midterm-exam/individualized-exams-pdf"
exams <- list.files(exam_path)

## Define mail server
smtp <- server(host = "smtp.uni.edu",
               port = 465,
               username = "dcohen@uni.edu",
               password = "ThisIsMySecretPassword2020!")

  ## Setup email template
  email_template <- envelope()

  ## Define from, to, and CC addresses
  email_template <- email_template %>%
    from("denis.cohen@uni.edu") %>%
    cc(
      c(
        "denis.cohen@uni.edu",
        "mneunhoe@uni.edu",
        "orittman@uni.edu"
      )
    )

  ## Define subject
  email_template <- email_template %>%
    subject("QM 2020 // Midterm Exam")

  ## Define email body in HTML
  html_body_template <-
    paste0(
      "<html><body>",
      "Dear ",
      "PLACEHOLDER_NAME",
      ",",
      "<br><br>",
      "attached to this email you find the QM 2020 Midterm Exam.",
      "<br><br>",
      "You have 90 minutes to complete the exam, plus 15 minutes to export your",
      " answers as a single PDF file and upload the file to ",
      "<a href='ADD_URL'>ILIAS</a> before 10h15.",
      "<br><br>",
      "The exam is open book; you can consult any materials you like. ",
      "You can use a calculator or statistical software to solve mathematical ",
      "problems.",
      "<br>",
      "You must not collborate with or seek help from others.",
      "<br><br>",
      "In case of questions or technical difficulties, you can contact us via ",
      "<a href='ADD_URL'>Zoom</a>.",
      "<br><br>",
      "Good luck!",
      "<br><br>",
      "Best,",
      "<br>",
      "Denis",
      "</body></html>"
    )


## ----R Code: Distributing individualized exams (step 2)-----------------------
## Start loop through students in student_roster
# for (i in seq_len(nrow(student_roster))) {
#   ## Retrieve file path for individualized exam
#   individualized_exam <- paste0(
#     exam_path,
#     "/exam_",
#     student_roster$login_id[i],
#     ".pdf"
#     )
#
#   ## Individualize HTML Body
#   html_body <- gsub("PLACEHOLDER_NAME",
#                     student_roster$first_name[i],
#                     html_body_template)
#
#   ## Compose individualized email and add attachment
#   email <- email_template %>%
#     to(paste0(student_roster$login_id[i], "@uni.edu")) %>%
#     html(html_body) %>%
#     attachment(path = individualized_exam, name = "QM2020-midterm.pdf")
#
#   ## Send email
#   smtp(email, verbose = TRUE)
# }
# ## End loop through students in student_roster
