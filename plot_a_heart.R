plot_a_heart <- function(){
        ## R-Code to generate a plot for the following equation
        ## y = x^(2/3) +/- sqrt(1 - x^2), which draws a heart
        ## In plain language, y equals x to the two-thirds plus or minus the 
        ## square-root of one minus x squared
        
        
        ## Generate the x data to be plotted - this creates 20,001 x values
        ## x cannot be less than -1 or greater than 1 (lower or higher will 
        ## create negative values in the square root... a no-no in math)
        tmp <- -1.0
        x <- data.frame()
        
        for (i in 1:20001){
                x[i,1] <- tmp
                tmp <- tmp + (1 / 10000)
        }
        
        ## I've created two y variables - one to handle the "plus" in the plus-
        ## or-minus statement and one to handle the minus.
        y1 <- data.frame()
        y2 <- data.frame()
        
        ## This loop generates the y values - the reason for the if statment is 
        ## for some reason, R did not like the negative values getting raised 
        ## to the 2/3 power
        for(i in 1:20001){
                y1[i,1] <- if(x[i,1] < 0){
                        (-x[i,1])^(2/3) + sqrt(1 - x[i,1]^2)
                } else {
                        (x[i,1])^(2/3) + sqrt(1 - x[i,1]^2)
                }
                
                y2[i,1] <- if(x[i,1] < 0){
                        (-x[i,1])^(2/3) - sqrt(1 - x[i,1]^2)
                } else {
                        (x[i,1])^(2/3) - sqrt(1 - x[i,1]^2)
                }
        }
        
        ## combine the values into a single data frame and name them
        final <- cbind(x, y1, y2)
        names(final) <- c("x", "ypos", "yneg")
        
        ## plot the result! I've removed the axes and frame to allow for a 
        ## nicer picture
        plot(x = final$x, y = final$ypos, ylim = c(-1, 1.5), col = "red", 
             xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
        points(x = final$x, y = final$yneg, col = "red")
}