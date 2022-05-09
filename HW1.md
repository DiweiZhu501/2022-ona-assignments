Exercise 1
================
Diwei Zhu
5/8/2022

``` r
# install.packages("tidyverse")
# install.packages("igraph")
# install.packages("tidygraph")
# install.packages("ggraph")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(tidygraph)
```

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     groups

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggraph)
```

## Loading data & pre-processing

``` r
connection <- read_csv("Connections.csv")
```

    ## Rows: 428 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): First Name, Last Name, Email Address, Company, Position, Connected On
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
view(connection)
```

Remove some needless columns and drop rows where company=NA.

P.S. I connected with many people who have the same name as I do (Diwei
Zhu) :) I removeed them to make sure we have unique label for each of my
connection

``` r
drops <- c("Email Address","Position","Connected On")
connection1 <- connection[ , !(names(connection) %in% drops)]
connection2 <- connection1[!is.na(connection1$Company), ]
connection3 <- connection2[!(connection2$'First Name'=="Diwei"),]  
```

Get list of compaies by frequency

``` r
connection3 %>% 
  count(Company) %>% 
  arrange(-n)
```

    ## # A tibble: 262 × 2
    ##    Company                                                 n
    ##    <chr>                                               <int>
    ##  1 McGill University - Desautels Faculty of Management    33
    ##  2 McGill University                                      29
    ##  3 Scotiabank                                              8
    ##  4 Sustainalytics                                          7
    ##  5 Deloitte                                                6
    ##  6 Desautels Capital Management                            6
    ##  7 Air Transat                                             5
    ##  8 McGill University - Faculty of Science                  5
    ##  9 Rogers Communications                                   5
    ## 10 Amazon                                                  4
    ## # … with 252 more rows

create unique labels

``` r
connection3 <- connection3 %>% mutate(
  label = paste(connection3$'First Name', connection3$'Last Name')
)
```

## create nodes

``` r
nodes <- connection3
nodes <- nodes %>% rowid_to_column('ID')
head(nodes, 10)
```

    ## # A tibble: 10 × 5
    ##       ID `First Name` `Last Name`           Company                        label
    ##    <int> <chr>        <chr>                 <chr>                          <chr>
    ##  1     1 Vanessa      McMullen              Shopify                        Vane…
    ##  2     2 Ethan        Bird                  Sleeping Bear Dunes National … Etha…
    ##  3     3 Radhika      Sharma                CrypConnect                    Radh…
    ##  4     4 Xuan         Chen                  Cboe Global Markets            Xuan…
    ##  5     5 MoRu         Tan                   Morgan Stanley                 MoRu…
    ##  6     6 Japmann      Sarin                 123Loadboard                   Japm…
    ##  7     7 Aydin        Sarraf                Ericsson                       Aydi…
    ##  8     8 Uzair        Ahmad                 Intelligems                    Uzai…
    ##  9     9 Franck       Benichou, M.A., M.Sc. Intact                         Fran…
    ## 10    10 Kristen      Chen                  McGill University - Faculty o… Kris…

## create edges

Replicate a dataframe and join

``` r
drops_ <- c("First Name","Last Name")
connection4 <- connection3[ , !(names(connection3) %in% drops_)]
connection5 <- connection4
col_names = colnames(connection4)
colnames(connection5) <- paste(col_names, "_", sep="")
full_joined <- tidyr::crossing(connection5, connection4)
```

Create edges list by keeping connections of the same company

``` r
link <- filter(full_joined, (full_joined$Company == full_joined$Company_) & (full_joined$label != full_joined$label_))

link <- link %>% 
  left_join(nodes, by = c("label" = "label")) %>% 
  rename(ID_a = ID)
link <- link %>% 
  left_join(nodes, by = c("label_" = "label")) %>% 
  rename(ID_b = ID)

edges <- select(link, ID_a, ID_b, Company)

head(edges,10)
```

    ## # A tibble: 10 × 3
    ##     ID_a  ID_b Company    
    ##    <int> <int> <chr>      
    ##  1   116   140 Accuracy   
    ##  2   140   116 Accuracy   
    ##  3   288   180 Air Transat
    ##  4    70   180 Air Transat
    ##  5   271   180 Air Transat
    ##  6   210   180 Air Transat
    ##  7   180   288 Air Transat
    ##  8    70   288 Air Transat
    ##  9   271   288 Air Transat
    ## 10   210   288 Air Transat

## Fitting model & visualization

``` r
network <- tbl_graph(nodes=nodes, edges=edges, directed=FALSE)
network
```

    ## # A tbl_graph: 414 nodes and 2246 edges
    ## #
    ## # An undirected multigraph with 262 components
    ## #
    ## # Node Data: 414 × 5 (active)
    ##      ID `First Name` `Last Name` Company                                label   
    ##   <int> <chr>        <chr>       <chr>                                  <chr>   
    ## 1     1 Vanessa      McMullen    Shopify                                Vanessa…
    ## 2     2 Ethan        Bird        Sleeping Bear Dunes National Lakeshore Ethan B…
    ## 3     3 Radhika      Sharma      CrypConnect                            Radhika…
    ## 4     4 Xuan         Chen        Cboe Global Markets                    Xuan Ch…
    ## 5     5 MoRu         Tan         Morgan Stanley                         MoRu Tan
    ## 6     6 Japmann      Sarin       123Loadboard                           Japmann…
    ## # … with 408 more rows
    ## #
    ## # Edge Data: 2,246 × 3
    ##    from    to Company    
    ##   <int> <int> <chr>      
    ## 1   116   140 Accuracy   
    ## 2   116   140 Accuracy   
    ## 3   180   288 Air Transat
    ## # … with 2,243 more rows

visualize the network

``` r
ggraph(network, layout = "graphopt") + 
  geom_edge_link(aes(color = Company), show.legend = FALSE) + 
  geom_node_point()+
  theme_graph()
```

![](HW1_files/figure-gfm/9-1.png)<!-- -->
