test_that(
    'code.count.normalization regression case "sum"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::code.count.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'sum',
            verbose = FALSE
            );

            expect_equivalent(result, NanoStringNorm.test.data$outputs$count.normalization.sum);
        }
    );

test_that(
    'code.count.normalization regression case "geo.mean"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::code.count.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'geo.mean',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$count.normalization.geo.mean);
        }
    );

test_that(
    'code.count.normalization errors on invalid normalization value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::code.count.normalization(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    'mean',
                    verbose = FALSE
                    );
                },
            regexp = 'Unimplemented'
            );
        }
    );

test_that(
    'code.count.normalization errors on NA normalization value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::code.count.normalization(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    NA,
                    verbose = FALSE
                    );
                },
            regexp = 'missing'
            );
        }
    );
