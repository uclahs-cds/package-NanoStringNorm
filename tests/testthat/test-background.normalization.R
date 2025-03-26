test_that(
    'background.normalization regression case "mean"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::background.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'mean',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.mean);
        }
    );

test_that(
    'background.normalization regression case "mean.2sd"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::background.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'mean.2sd',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.mean.2sd);
        }
    );

test_that(
    'background.normalization regression case "max"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::background.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'max',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.max);
        }
    );

test_that(
    'background.normalization errors on invalid normalization value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::background.normalization(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    'garbage',
                    verbose = FALSE
                    );
                },
            regexp = 'Unimplemented'
            );
        }
    );
