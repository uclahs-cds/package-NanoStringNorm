test_that(
    'predict.concentration regression case', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::predict.concentration(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            is.log = TRUE,
            take.log = TRUE,
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$predicted.concentration);
        }
    );