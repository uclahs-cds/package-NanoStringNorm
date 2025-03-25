test_that(
    'output.formatting regression case', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::output.formatting(
            NanoStringNorm.data$inputs$x,
            NanoStringNorm.data$inputs$anno,
            round.values = TRUE,
            take.log = TRUE,
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.data$outputs$roundT.logT);
        }
    );

test_that(
    'output.formatting errors with invalid rounding value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::output.formatting(
                    NanoStringNorm.data$inputs$x,
                    NanoStringNorm.data$inputs$anno,
                    round.values = 'garbage',
                    verbose = FALSE
                    );
                },
            regexp = 'Round'
            );
        }
    );

test_that(
    'output.formatting errors with NA rounding value', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm:::output.formatting(
                    NanoStringNorm.data$inputs$x,
                    NanoStringNorm.data$inputs$anno,
                    round.values = NA,
                    verbose = FALSE
                    );
                },
            regexp = 'Round'
            );
        }
    );

test_that(
    'output.formatting prints message with z-score normalized data', {
        load('data/NanoStringNorm.Rda');

        expect_output(
            {
                NanoStringNorm:::output.formatting(
                    NanoStringNorm.data$inputs$x,
                    NanoStringNorm.data$inputs$anno,
                    round.values = TRUE,
                    take.log = TRUE,
                    OtherNorm = 'zscore',
                    verbose = TRUE
                );
            },
            regexp = 'zscore'
            );
        }
    );

test_that(
    'output.formatting prints message with rank-normal normalized data', {
        load('data/NanoStringNorm.Rda');

        expect_output(
            {
                NanoStringNorm:::output.formatting(
                    NanoStringNorm.data$inputs$x,
                    NanoStringNorm.data$inputs$anno,
                    round.values = TRUE,
                    take.log = TRUE,
                    OtherNorm = 'rank.normal',
                    verbose = TRUE
                    );
                },
            regexp = 'rank.normal'
            );
        }
    );
