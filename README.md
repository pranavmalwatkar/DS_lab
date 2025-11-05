# DS_lab

1)R programming

2)Use the Iris dataset and train a Logistic
Regression model to classify whether a
flower is of species Iris-setosa or not.
Evaluate model accuracy, precision, and
recall.

3)Load the Titanic dataset, perform basic
preprocessing (handle missing values,
encode categorical variables), and train a
Decision Tree classifier to predict the
model. Plot the confusion matrix.

4)Use the Naive Bayes classifier to perform
sentiment analysis on a dataset of movie
reviews, classifying them as either positive
or negative based on the review text.

5)A company wants to predict the salary of
employees based on their years of
experience. The company has collected data
on the salaries and experience of 100
employees. Use simple linear regression to
model the relationship between salary and
experience.

6)Use a dataset with features like square
footage vs. house price. Implement a simple
linear regression model to predict house
prices. Plot the regression line and calculate
MSE and R² score.

7)Load the tissue_gene_expression dataset.
Run a k-means clustering on the data
with K=7. Make a table comparing the
identified clusters to the actual tissue types.
Run the algorithm several times to see how
the answer changes.

8)Using grocery dataset with minimum
support to 0.001 and minimum confidence
of 0.8 build a frequent pattern tree (FP-
Tree). Show for each transaction how the
tree evolves.

9)Use the Apriori algorithm on the grocery
dataset with minimum support to 0.001 and
minimum confidence of 0.8 indicate the top
5 association rules that are generated and
highlight the strong ones, sort them by
confidence.

10)Use the FP-Growth Algorithm to perform
Market Basket Analysis on a retail dataset.
You will extract frequent item sets and
generate association rules to discover
purchasing patterns and help improve sales
strategies.

11)Create Bar Chart-Stacked taking x axis as
Month and Y axis as Revenue and add
Legend
colors &lt;- c(&quot;green&quot;,&quot;orange&quot;,&quot;brown&quot;)
months &lt;-
c(&quot;Mar&quot;,&quot;Apr&quot;,&quot;May&quot;,&quot;Jun&quot;,&quot;Jul&quot;)
regions &lt;- c(&quot;East&quot;,&quot;West&quot;,&quot;North&quot;)

12)Read the file moviesData.csv to create a bar
chart of critics_score for the first 10 movies.
Save the plot.

13)Use the data set “mtcars”and create boxplot
for “mpg” and “cyl” columns 

Q1 8)anser:
# R Program to find the Digital Root of a number using a repeat loop.
# The digital root is the single-digit value obtained by an iterative
# process of summing the digits, on each result, until only one digit remains.

# --- Helper Function: sum_digits ---
# This function takes a number and returns the sum of its individual digits.
sum_digits <- function(n) {
  # We use the absolute value to handle potential negative inputs, though
  # digital root is typically defined for non-negative integers.
  if (n < 0) {
    n <- abs(n)
  }
  
  sum_val <- 0
  # Convert the number to an integer to ensure correct modulo and integer division
  num_int <- as.integer(n)

  # Use a while loop to extract and sum digits
  while (num_int > 0) {
    # Get the last digit (remainder when divided by 10)
    digit <- num_int %% 10
    sum_val <- sum_val + digit
    # Remove the last digit (integer division by 10)
    num_int <- num_int %/% 10
  }
  return(sum_val)
}

# --- Main Function: find_digital_root_repeat ---
# This function uses the mandatory 'repeat' loop to reduce the number
# to its single-digit digital root.
find_digital_root_repeat <- function(number) {
  # Basic input validation
  if (!is.numeric(number) || length(number) != 1) {
    stop("Input must be a single numeric value.")
  }

  n <- as.integer(abs(number))
  
  # Handle 0 case immediately
  if (n == 0) {
    return(0)
  }

  cat(paste0("Starting Digital Root calculation for: ", n, "\n"))
  
  # The 'repeat' loop executes indefinitely until a 'break' condition is met.
  repeat {
    # Check if the number is already a single digit (less than 10)
    if (n < 10) {
      cat("Reduction complete. Final digit reached.\n")
      break # Exit the loop
    }
    
    # If the number is still two or more digits, calculate the sum of its digits
    n <- sum_digits(n)
    cat(paste0("  -> Sum of digits is: ", n, "\n"))
  }

  return(n)
}

# --- Examples of Usage ---

num1 <- 987654321
root1 <- find_digital_root_repeat(num1)
cat(paste0("\nResult for ", num1, ": ", root1, "\n\n")) # Expected: 9 (9*10 + 10 = 45, 4+5=9)

num2 <- 49
root2 <- find_digital_root_repeat(num2)
cat(paste0("\nResult for ", num2, ": ", root2, "\n\n")) # Expected: 4 (4+9=13, 1+3=4)

num3 <- 12345
root3 <- find_digital_root_repeat(num3)
cat(paste0("\nResult for ", num3, ": ", root3, "\n\n")) # Expected: 6 (1+2+3+4+5=15, 1+5=6)

num4 <- 7
root4 <- find_digital_root_repeat(num4)
cat(paste0("\nResult for ", num4, ": ", root4, "\n")) # Expected: 7


7)
library(readr)
library(writexl)
library(readxl)

# 1. Create sample Data Frame (equivalent to pd.DataFrame)
data_export <- data.frame(
  Name = c('John', 'Alice', 'Bob'),
  Age = c(25, 30, 22),
  City = c('Delhi', 'Mumbai', 'Pune'),
  stringsAsFactors = FALSE
)

# 2. Export data
# Export to CSV (index=TRUE in Python -> row.names=TRUE in R)
# Note: Base R's write.csv is used here to ensure row names are written
# as the first column, mimicking the Pandas behavior when index=TRUE.
write.csv(data_export, 'sample.csv', row.names = TRUE)

# Export to Excel (index=FALSE in Python)
# We use writexl::write_xlsx and pass the data.frame inside a list.
# R data frames do not export row names by default unless explicitly told to.
write_xlsx(list(Sheet1 = data_export), 'sample.xlsx')

# Export to TXT (sep='\t', index=FALSE in Python)
# readr::write_tsv is the equivalent of writing a tab-separated file without row names.
write_tsv(data_export, 'sample.txt')


# 3. Import data back

# Import from CSV
# Using base R's read.csv and telling it the row names are in the first column (col index 1)
csv_data <- read.csv('sample.csv', row.names = 1, stringsAsFactors = FALSE)

# Import from Excel
excel_data <- read_excel('sample.xlsx')

# Import from TXT (tab-separated)
txt_data <- read_tsv('sample.txt', show_col_types = FALSE)


# 4. Print results (using cat and print for clean output)
cat("Q7: Imported Data from CSV:\n")
# The row names (1, 2, 3) are now actual row names in R, not a column.
print(csv_data)

cat("\nImported Data from Excel:\n")
# The data is imported as a tibble (from readxl), which is similar to a data frame
print(excel_data)

cat("\nImported Data from TXT:\n")
# The data is imported as a tibble (from readr)
print(txt_data)

# Clean up created files (optional)
# file.remove(c('sample.csv', 'sample.xlsx', 'sample.txt'))