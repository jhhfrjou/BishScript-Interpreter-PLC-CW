# BishScript User Manual

BishScript is a Conjunctive Query Language, that attempts to bridge the gap between SQL like queries and first-order logic queries. The syntax of BishScript allows for quick queries to be written whilst remaining easy to read and understand at first look. 

## How to Write

Comments follow the double forward slash `//`. Imports and Queries are all followed by a vertical pipe `|`
```
import File.csv | // This is a comment
```

Keywords:
Here is a list of the keywords of the language and will be explained in context later. Obviously because they are keywords they can’t be used as a variable name.
`import`    `as`     `take`     `where`      `exists`

### Importing CSV Files

CSV files are imported using the ‘import’ keyword followed by the filename then ‘as’ and the name you would like to use to refer to the CSV file in the condition. The name you use as an alias must start with a Capital letter. In addition, the language currently doesn’t support importing csv files that start with a number.

`import example.csv as E |` is valid 

`import explained.csv as e|` is not valid

To make things easier, CSV files that starts with a capital letter can use the initial file name to refer to that file inside of the statement.

`import Example.csv |`  is valid and the file will be referred to as Example in the condition.

`import example.csv |` is not valid

### Queries
Queries are the heart of the language and is where the actual stuff takes place.
These are broken down into two parts. The take and the where, which is explained below.

#### Take
This is where you select the variable names that will be used later in the where part of the query. To do this write `take`, then add a list of variables separated by commas, in square brackets.
```
take [x1] …
take [x1, x2, x3] …
```
#### Where
After the list the `where` keyword is used and the condition to the query is inputted.
There are 3 types of conditions that are linked with conjoins.

##### File/Table References

The first and most important one is one that refers to tables. This is where you introduce a table to the query and assign the columns to variable names. This is by using the `Alias` followed by a list of variables in order `[x1, x2]`

```
take [x1,x2] where FileName[x1,x2] | 
``` 
will read in the file, assigns the first column of the file to x1 and the second column into x2.

In addition to this, when using the same variable twice in the same reference, it will check for equality in both columns. 

```
take [x] where FileName[x,x] |
```

Without at least one of these in the query, the script is useless, since the variables are not assigned to a column and will result in an error. See Errors.

##### Conjoins

This is a binary operation that combines two conditions together using the symbol `^` between the conditions  It combines all combinations of the result of both sides. 
```
take [x1, x2] where FileName[ x1] ^ FileName2[x2] |
```
Like with file references with repeated variables, equality is checked when there is a repeated variable.
```
take [x] where R[x] ^ S[x] |
```

##### Existential Quantification.

The existential quantifier is a way of introducing bound variables. By introducing a bound variable it allows you to check for a property, or just not have a certain column printed at the end. To do this you use the keyword `exists`, then a list of variables you want to initialise a `.` then the condition.
```
take  [x] where exists [y,z] . FileName[x,y,z] | 
```
The condition of the existential, quantifier means the first two statements are equal while the second one is not
```
take [x] where exists [y] . File [x,y] ^ File2 [x] | 
take [x] where exists [y] . (File [x,y] ^ File2 [x]) |
take [x] where (exists [y] . File [x,y]) ^ File2 [x] |
```


With multiple exists statements ensure the introduced variable have unique names between each statements 
```
take [x] where (exists [y] . File[x,y]) ^ (exists [y] . File [y,x]) | // will return an incorrect answer
take [x] where (exists [y] . File[x,y]) ^ (exists [z] . File [z,x]) | // will return the correct answer
```

Because of the precedence, we have added brackets to our language. When there are multiple `exists` statements in a query, it is useful to enclose each `exists` statements in brackets to avoid ambiguity.

##### Equals

The equals `=` condition takes 2 variables and compares if those 2 columns of the table are equal then returns only the rows where those 2 variables are equal.
```
take [x1, x2] where R[x1, x2] ^ x1 = x2 |
```


## Errors

The ordering of the errors is the order they are checked, i.e. No Statement error is the first that is checked.

| Error                                                                                  | Explanation                                                                                                                                                         | Example                                                                                                                    |
|----------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| Wasn’t Expecting a \[character name\] (that symbol) at (line number, column number)      | There’s a parsing error and that symbol doesn’t fit into the grammar                                                                                                | import s.csv \| will give “Wasn't Expecting a pipe (\|) at: (line 1, column 13)” since it requires an `as Alias|` afterwards. |
| Filename: openFile: does not exist (No such file or directory)                         | Can’t find file, check if file is in the directory or you haven’t misspelled the file name.                                                                         |                                                                                                                            |
| No Statement                                                                           | The program has reached the end of the script and has found no take statement.                                                                                      | import R.csv \|                                                                                                             |
| Nothing is outputted                                                                   | There are no variables to output. The list after take is empty. NOTE this is different to an empty output.                                                          | import A.csv \| take [] where exists [x1]. A [x1] \|                                                                         |
| The references to a file are not connected to file.                                    | The file/table has not been imported or                                                                                                                             | take [x1, x2] where R [x1, x2] \|                                                                                           |
| Not all variables used/ Returning Bound Variables/ Bound Variables Not Initialised yet | This can be cause by multiple issues.  Listed below                                                                                                                 |                                                                                                                            |
|                                                                                        | First it could be having an outputted variable that has not be used in the query. Ensure every variable before the where is also after the where.                   | import R.csv \| take [x1, x2] where R[x1] \|                                                                                 |
|                                                                                        | It could also because an outputted variable is also initialised in an existential binding.                                                                          | import R.csv \| take [x1, x2] where exists [x2]. R [x2, x1] \|                                                               |
|                                                                                        | The final reason this error is produced is when you use a bounded variable outside  the existential clause.                                                         | import R.csv \| import S.csv \| take [x1, x2] where R [x1, x2, z] ^ exists [z]. S [x1, x2] \|                                 |
| The number of columns in the csv is not equal to the number in the references          | Means when referring that csv file, the number of columns given does not match the number of variables. It will also throw this error if an empty file is inputted. | A.csv: A, B, C import A.csv \| take [x1, x2] where A [x1, x2] \|                                                              |
| Map.!: given key is not an element in the map                                          | When you have a condition with no references to any tables, only equality and existential quantifiers.                                                              | take [x1, x2] where x1 = x2 \| or take [x1] where exists [x]. x = x1 \|                                                      |

## Appendix

Here are the solutions to the set problems written in BishScript

### Problem 1

```
import A.csv as A |
import B.csv as B  |
take [x1,x3,x2,x4] where A [x1,x2] ^ B [x3, x4]|
```

### Problem 2

```
import A.csv as A |
import B.csv as B  |
take [x1,x2,x3] where A [x1,x2] ^ B [x2, x3]|
```

### Problem 3

```
import P.csv as P |
import Q.csv as Q  |
take [x1,x2] where P [x1] ^ Q [x2] ^ x1 = x2|
```
### Problem 4

```
import R.csv as R |
take [x1] where exists [z] . R [x1,z]|
```
### Problem 5

```
import A.csv as A |
import B.csv as B |
take [x1,x2] where exists [z] . A [x1,z] ^ B [z , x2]|
```
### Problem 6

```
import R.csv as R|
import S.csv as S|
take [x1] where R [x1] ^ exists [z] . S [z] |
```
Note instead of returning an empty output it will throw an error because it's an empty file.
### Problem 7

```
import R.csv|
take [x1,x2] where exists [z1,z2] . R [x1,z1] ^ R [z1,z2] ^ R [z2,x2] |
```
### Problem 8

```
import R.csv |
take [x1,x2,x3,x4,x5] where R [x1, x2] ^ R[x2, x3] ^ R[x3, x4] ^ R[x4, x5] ^ x1 = x5 |
```
### Problem 9

```
import A.csv |
import B.csv |
import C.csv |
take [x1,x2] where exists [z1,z2] .  A[x1, z1] ^ B[z1, z2] ^ C[z2, x2] |
```
### Problem 10

```
import S.csv |
take [x1,x2,x3] where S[x1, x2, x3] ^ (exists [y1,y2] . S[y1,y1,y2]) ^ exists [z1,z2] . S[z1,z2,z2] |
```
