test_that(
    'get.sample.summary.stats regression case', {
        load('data/NanoStringNorm.Rda');

        result <- get.sample.summary.stats(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno
            );

        expect_equivalent(
            result,
            NanoStringNorm.test.data$outputs$sample.summary
            );
        }
    );
