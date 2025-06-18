test_that(
    'sample.content.normalization regression case "top mean"', {
        load('data/NanoStringNorm.Rda');

        result <- sample.content.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'top.mean',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$top.mean);
        }
    );

test_that(
    'sample.content.normalization regression case "top geo mean"', {
        load('data/NanoStringNorm.Rda');

        result <- sample.content.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'top.geo.mean',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$top.geo.mean);
        }
    );

test_that(
    'sample.content.normalization regression case "total.sum"', {
        load('data/NanoStringNorm.Rda');

        result <- sample.content.normalization(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            'total.sum',
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$total.sum);
        }
    );
