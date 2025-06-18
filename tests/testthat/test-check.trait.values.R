test_that(
    'check.trait.values handles valid input', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::check.trait.values(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            traits = NanoStringNorm.test.data$inputs$trait
            );

        expect_true(result);
        }
    );

test_that(
    'check.trait.values handles invalid input with "0" values', {
        load('data/NanoStringNorm.Rda');

        invalid.trait <- NanoStringNorm.test.data$inputs$trait;
        invalid.trait[1, 2] <- 0;

        expect_error(
            {
                NanoStringNorm:::check.trait.values(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    traits = invalid.trait
                    );
                },
            regexp = 'numeric variables'
            )
        }
    );

test_that(
    'check.trait.values handles invalid input with different dimensions', {
        load('data/NanoStringNorm.Rda');

        invalid.trait <- NanoStringNorm.test.data$inputs$trait;
        invalid.trait <- invalid.trait[-1, ];

        expect_error(
            {
                NanoStringNorm:::check.trait.values(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    traits = invalid.trait
                    );
                },
            regexp = 'number of traits'
            )
        }
    );

test_that(
    'check.trait.values handles invalid input with different row order', {
        load('data/NanoStringNorm.Rda');

        invalid.trait <- NanoStringNorm.test.data$inputs$trait;
        invalid.trait <- invalid.trait[order(rownames(invalid.trait)), ];

        expect_error(
            {
                NanoStringNorm:::check.trait.values(
                    NanoStringNorm.test.data$inputs$x,
                    NanoStringNorm.test.data$inputs$anno,
                    traits = invalid.trait
                    );
                },
            regexp = 'same samples'
            )
        }
    );
