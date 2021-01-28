
descriptions = list(
  tp1_data_plot = paste('The above plot displays measured dilution fraction',
                        'on the x-axis and cell concentration on the y-axis.',
                        'The different panels indicate the different counting methods.',
                        'Each point represents an average of the observations',
                        'for a particular replicate sample.',
                        'The black diagonal line represents the fitted',
                        'proportional model. The above button toggles',
                        'the fitted flexible (polynomial) model,',
                        'which displays the fitted model (solid lines) and',
                        'associated prediction intervals (dotted lines)',
                        'If the measured cell concentration is truly proportional',
                        'to the measured dilution fraction, we would expect',
                        'the flexible model (solid colored line) to fall',
                        'close to the proportional model (solid black line).'),
  
  tp1_raw_data_plot = paste('The above plot displays the (non-aggregated) raw data.',
                            'Cell concentration is displayed as a function of the',
                            'target (or measured, if present) dilution fraction.'),
  
  tp1_residual_plot = paste('The top row of the above plot shows the raw residuals',
                            'for the proportional model.',
                            'The second row shows the smoothed residuals, which',
                            "indicate the difference between the flexible model's fitted",
                            "values and the proportional model's fitted values.",
                            'The bottom row plots the smoothed residuals (row 2),',
                            'which have been scaled by the dilution fraction (x-axis).')
)