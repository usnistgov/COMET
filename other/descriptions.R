
descriptions = list(
  tp1_data_plot = paste('The above plot displays measured (or target) dilution fraction',
                        'on the x-axis and cell concentration on the y-axis.',
                        'The different panels indicate the different counting methods.',
                        'Each point represents an average of the observations',
                        'for a particular replicate sample.',
                        'The black diagonal line represents the fitted',
                        'proportional model. The above button toggles',
                        'prediction intervals computed using the',
                        'fitted flexible (polynomial) model.',
                        "Prediction intervals that don't overlap",
                        'with the proportional model suggests potential non-linearity.'),
  
  tp1_raw_data_plot = paste('The above plot displays the (non-aggregated) raw data.',
                            'Cell concentration is displayed as a function of the',
                            'target (or measured, if present) dilution fraction.'),
  
  tp1_residual_plot = paste('The top row of the above plot shows the raw residuals',
                            'for the proportional model.',
                            'The second row shows the smoothed residuals, which',
                            "indicate the difference between the flexible model's fitted",
                            "values and the proportional model's fitted values.",
                            'The bottom row plots the smoothed residuals (row 2),',
                            'which have been scaled by the dilution fraction (x-axis).'),
  
  help_pis = paste('Each PI gives a different measure of proportional fit for the data.',
                   '"Smoothed" residuals are computed from the difference between the',
                   'proportional model and the flexible model. Large values suggest',
                   'that the data is not proportional.',
                   '"Scaled" residuals have been scaled by the dilution fraction so',
                   'that a 10% deviation, say, at two different dilution fractions are',
                   'penalized equally in PI term (generally recommended).',
                   'PIs using squared error penalize large residuals to a greater extent',
                   'than PIs using absolute error.')
)