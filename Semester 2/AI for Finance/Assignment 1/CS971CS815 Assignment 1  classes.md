#### Assignment - Portfolio Optimisation using GAs

In this assignment you will investigated the development of a GA-based solution to optimise the weightings of a portfolio of assets and is composed of two parts:

##### Part 1

The focus of the first part of the assignment is on the optimisation of the portfolio using a GA. To begin with, manually choose a set of assets to work with - which ones you select are entirely up to you, but aim to work with around 10 and also try to pick ones which don't all exhibit the same behaviour (e.g. maybe take them from different sectors).

Then use a genetic algorithm (and the GA package) to identify the optimal set of weights for your portfolio which gives the best balance of risk versus return.

Having done this you then need to conduct a detailed evaluation of the portfolio by looking at how well it might perform in the "future", and comparing it with other possible portfolios (particularly a balanced portfolio, where all weights are even, and a set of randomly generated portfolios).

Finally (for this part) try exploring different weightings of risk and return. The idea behind this it to try and emulate a multi-objective approach and you will need to think about how it is possible to modify the fitness function to create differently balanced portfolios - e.g. at the extremes might be ones which only care about maximising returns or are very risk averse, and in between might be ones which prioritise risk over return and vice-versa. How do the weights and the performance of these portfolios compare with each other and your other solutions?  

##### Part 2

Once you have done this successfully, then try extending your solution to select the set of assets for your GA to work on. The pool from which you select should be reasonably large (minimum of 50) and to begin with you may identify the selection criteria so that this GA operates independently of the one in part 1 (but would pass on the set of identified assets to be weighted by the GA from part 1). Once you have this working (and if you are feeling ambitious!) then investigate how these might work together (working out an efficient evaluation mechanism is the key to this part).  

Further details along with the associated marks are given below:  

##### Assessment criteria:

-   Construction of a portfolio using the GA package (3 marks)  
    
-   Evaluation of the portfolio on unseen "future" data (4 marks)

-   To do this you will need to keep back some of your data (like a standard train/test split). For example, you might have trained your portfolio using data from 2019 to give you a set of weights. Then, keeping these weights constant, use them to calculate the returns from 2020  data. This should be a very simple calculation. Include your observations on this performance.  
    

-   Comparison of the evolved portfolio with balanced and random portfolios (5 marks)

-   Is the portfolio you evolved any good?  Compare and comment on the performance against an evenly balanced portfolio and the average of several runs of randomly generated portfolios. You could use run this comparison on both the training and the test data.  
    

-   Creation and evaluation of portfolios with differently balanced risk and return (to emulate a multi-objective approach) (6 marks)
-   Within your fitness function your risk and return values will be evenly balanced. Modify this to create a number of differently balanced portfolios which give different preferences to risk or return. Report on the performance of these portfolios by considering how the weights and the performance of these portfolios compare to balanced (and other) solutions.

Using GAs to select the assets (7 marks)

-   Use a GA to pick the assets from a much larger set using a defined criteria. Report on the performance of this set (and make reference to the performance of portfolios using the approaches in part 1.
-   Integrate the two approaches so that the GA which is selecting the assets uses feedback from the GA that is optimising the portfolio. Again report on the performance of this approach, and compare it with others you have run. 

##### Submission Requirements

Your submission should consist of a report created using R markdown which contains _all_ the code produced along with the results and observations on these. The report should be in pdf format, a **maximum of 10 pages** and follow the following structure (page counts for each section are a suggested maximum and included for guidance):

-   Construction of a portfolio using the GA package (2 pages)  
    -   Brief overview of the assets selected and why, the fitness function employed, GA parameters, plot of performance, evolved weights and details of performance (risk and return)  
        
-   Evaluation of the portfolio on unseen ”future” data (1 page)
    -   Choice of test data, performance of the portfolio over the period, comments on performance  
        
-   Comparison of the evolved portfolio with evenly weighted and random portfolios (1 page)
    -   Construction of weights, performance of the portfolios over the period, comments on performance  
        
-   Creation and evaluation of portfolios with differently balanced risk and return (3 pages)
    -   Details of changes to fitness function, evolved weights, and performance (risk and return over train and test periods, and train/test performance against portfolios created earlier, including random portfolios)  
        
-   Using GAs to select the assets (3 pages)
    -   Details of pool of assets being considered, criteria for selection and evaluation (fitness function), and train/test performance of finally selected portfolio compared against original selection and random portfolios  
        

**Both** the original .Rmd **and** generated .pdf should be submitted. Submissions missing either of these will not be marked.

##### Some useful links on R Markdown:

-   [https://rpubs.com/brandonkopp/RMarkdown](https://rpubs.com/brandonkopp/RMarkdown)
-   [https://rmarkdown.rstudio.com/authoring\_quick\_tour.html](https://rmarkdown.rstudio.com/authoring_quick_tour.html)
-   [https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)
-   [https://www.dataquest.io/blog/r-markdown-guide-cheatsheet/](https://www.dataquest.io/blog/r-markdown-guide-cheatsheet/)
-   [https://ourcodingclub.github.io/tutorials/rmarkdown/](https://ourcodingclub.github.io/tutorials/rmarkdown/)