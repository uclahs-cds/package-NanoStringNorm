test_that(
    'NanoStringNorm regression case "none"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::NanoStringNorm(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            verbose = FALSE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$none);
        }
    );

test_that(
    'NanoStringNorm regression case "none.matrix"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::NanoStringNorm(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            verbose = FALSE,
            return.matrix.of.endogenous.probes = TRUE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$none.matrix);
        }
    );

test_that(
    'NanoStringNorm regression case "random"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::NanoStringNorm(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            CodeCount = 'geo.mean', 
            Background = 'mean.2sd', 
            SampleContent = 'top.geo.mean', 
            log = TRUE, 
            round = TRUE, 
            verbose = FALSE,
            predict.conc = TRUE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$random);
        }
    );

test_that(
    'NanoStringNorm regression case "random" with annotations in x', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::NanoStringNorm(
            data.frame(
                NanoStringNorm.test.data$inputs$x,
                NanoStringNorm.test.data$inputs$anno
                ),
            anno = NA, 
            CodeCount = 'geo.mean', 
            Background = 'mean.2sd', 
            SampleContent = 'top.geo.mean', 
            log = TRUE, 
            round = TRUE, 
            verbose = FALSE,
            predict.conc = TRUE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$random);
        }
    );
