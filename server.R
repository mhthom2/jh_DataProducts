#
# This web application predicts the amount timber that can be harvested from 
# a black cherry tree with the given girth, or diameter at the height of 4'6", 
# and the height of the tree. 
#

library(shiny)
library(plotly)
library(reshape2)

set.seed(0)

# Black Cherry tree data
data(trees)

# Make volume prediction; plot data, model's Regression Plane, and prediction
function(input, output, session) {
        
        # Build linear model for data
        mdl <- lm(Volume ~ ., data=trees)  
        
        # Reactive function for making volume prediction
        mdlPrediction <- reactive({
                
                # Get girth and height inputs
                girthInput <- input$girth
                heightInput <- input$height

                # Predict magnitude for given inputs
                predict(mdl, newdata=data.frame(Girth=girthInput,
                                                Height=heightInput))
        })
        
        # Return value of the prediction
        output$prediction <- renderText({
                mdlPrediction()
        })
        
        # Plot data, regression plane, and new prediction
        output$plot3D <- renderPlotly({
                
                # Get girth and height inputs
                girthInput <- input$girth
                heightInput <- input$height
                # Predict volume with these values 
                volume <- predict(mdl, newdata=data.frame(Girth=girthInput,
                                                          Height=heightInput))
                
                # Display data on 3D scatter plot  
                tree_plot <- plot_ly(trees, x=~Girth, y=~Height, z=~Volume, 
                                     type='scatter3d', 
                                     mode='markers',
                                     name='Data')
                
                # Create surface representing regression plane 
                
                # Source: https://stackoverflow.com/questions/38331198/add-regression-plane-to-3d-scatter-plot-in-plotly
                
                # Number of samples for each feature
                num <- 50
                
                # x_axis increment 
                x_reso <- (max(trees$Girth) - min(trees$Girth)) / num
                y_reso <- (max(trees$Height) - min(trees$Height)) / num
                
                #Setup Axis
                axis_x <- seq(min(trees$Girth)-x_reso, max(trees$Girth)+x_reso, by=x_reso)
                axis_y <- seq(min(trees$Height)-y_reso, max(trees$Height)+y_reso, by=y_reso)
                
                #Sample points
                lm_surface <- expand.grid(Girth=axis_x, Height=axis_y)
                lm_surface$Volume <- predict.lm(mdl, newdata=lm_surface)
                lm_surface <- acast(lm_surface, Girth ~ Height, value.var='Volume') 
                
                # Add regression plane to plot
                tree_plot <- add_trace(p=tree_plot,
                                       z=lm_surface,
                                       x=axis_x,
                                       y=axis_y,
                                       type = "surface",
                                       name='Regression Plane')
                
                # Add prediction point
                tree_plot <- add_markers(p=tree_plot,
                                        x=girthInput,
                                        y=heightInput,
                                        z=volume,
                                       #mode='markers',
                                        symbols='x',
                                        color='red',
                                        name='Prediction')
                # Return plot 
                tree_plot
    })

}
