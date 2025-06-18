test_that(
    'other.normalization regression case "zscore"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::other.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            OtherNorm = 'zscore',
            verbose = FALSE,
            genes.to.fit = 'endogenous'
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.zscore);
        }
    );

test_that(
    'other.normalization regression case "quantile"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::other.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            OtherNorm = 'quantile',
            verbose = FALSE,
            genes.to.fit = 'endogenous'
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.quantile);
        }
    );

test_that(
    'other.normalization regression case "rank.normal"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::other.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            OtherNorm = 'rank.normal',
            verbose = FALSE,
            genes.to.fit = 'endogenous'
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$normalized.rank.normal);
        }
    );

test_that(
    'other.normalization errors with invalid "OtherNorm" value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::other.normalization(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    OtherNorm = 'garbage',
                    verbose = FALSE,
                    genes.to.fit = 'endogenous'
                    );
                },
                regexp = 'OtherNorm'
            );
        }
    );
