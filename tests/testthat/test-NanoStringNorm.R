test_that(
    'NanoStringNorm regression case "none"', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm(
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

        result <- NanoStringNorm(
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

        result <- NanoStringNorm(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            CodeCount = 'geo.mean', 
            Background = 'mean.2sd', 
            SampleContent = 'top.geo.mean', 
            take.log = TRUE,
            round.values = TRUE,
            verbose = FALSE,
            predict.conc = TRUE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$random);
        }
    );

test_that(
    'NanoStringNorm regression case "random" with annotations in x', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm(
            data.frame(
                NanoStringNorm.test.data$inputs$x,
                NanoStringNorm.test.data$inputs$anno
                ),
            anno = NA,
            CodeCount = 'geo.mean', 
            Background = 'mean.2sd', 
            SampleContent = 'top.geo.mean', 
            take.log = TRUE, 
            round.values = TRUE, 
            verbose = FALSE,
            predict.conc = TRUE
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$random.combined);
        }
    );

test_that(
    'NanoStringNorm errors if no annotations are included', {
        load('data/NanoStringNorm.Rda');

        expect_error(
            {
                NanoStringNorm(
                    NanoStringNorm.test.data$inputs$x,
                    anno = NA,
                    verbose = FALSE
                    );
                },
            regexp = 'annotation'
            );
        }
    );

test_that(
    'NanoStringNorm errors with invalid field names', {
        load('data/NanoStringNorm.Rda');

        names(NanoStringNorm.test.data$inputs$x)[1] <- 'Code__Class';
        expect_error(
            {
                NanoStringNorm(
                    NanoStringNorm.test.data$inputs$x,
                    anno = NA,
                    verbose = FALSE
                    );
                },
            regexp = 'Code.Class'
            );
        }
    );

test_that(
    'NanoStringNorm errors with "Code.Class" factor', {
        load('data/NanoStringNorm.Rda');

        x <- data.frame(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            stringsAsFactors = FALSE
            );
        x$Code.Class <- as.factor(x$Code.Class);

        expect_error(
            {
                NanoStringNorm(
                    x,
                    anno = NA,
                    verbose = FALSE
                    );
                },
            regexp = 'factor'
            );
        }
    );
