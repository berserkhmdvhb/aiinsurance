server <- function(session, input, output) {
  preds <- reactive({
    if (input$model_selected == "Logistic Regression"){
      actual <- aiinsurance::insurance_test$outcome
      model_glm <- aiinsurance::glmnet_fit_hmd(aiinsurance::insurance_train,
                                  target = "outcome",
                                  family = "binomial")
      aiinsurance::glmnet_predict_hmd(model_glm,
                         data = aiinsurance::insurance_test,
                         target = "outcome",
                         type = "binomial")
    }
    else if (input$model_selected == "Random Forest"){
      model_random_forest <- aiinsurance::rf_fit_hmd(aiinsurance::insurance_train,
                                        ntree = 300,
                                        mtry = 10,
                                        proximity = TRUE,
                                        importance = FALSE)

      aiinsurance::rf_predict_hmd(data=aiinsurance::insurance_test,
                     fit=model_random_forest)
    }
  }) |> bindCache(input$model_selected) |> bindEvent(input$run_plot)


  plot_eval <- reactive({
    actual <- insurance_test$outcome
    if (input$evaluation_selected == "ROC Curve"){
      pred_proba <- get_pred_proba(preds())
      roc_obj <- roc_obj_cal(actual, pred_proba)
      plot_roc_curve(roc_obj)
    }
    else if (input$evaluation_selected == "Confusion Matrix"){
      print(preds()$predictions)
      eval_glm <- aiinsurance::eval_hmd(actual,
                                        preds()$predictions)
      eval_glm$confusion_matrix_plot
    }

  })  |> bindCache(input$evaluation_selected) |> bindEvent(input$run_plot)


  output$evaluation_plots <- renderPlot({
    plot_eval()
  })





}
