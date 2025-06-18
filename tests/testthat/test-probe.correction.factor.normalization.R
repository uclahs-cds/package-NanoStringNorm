test_that(
    'probe.correction.factor.normalization errors with NA factor', {
        load('data/probe.correction.Rda');

        expect_error(
            {
                probe.correction.factor.normalization(
                    x = probe.correction.test.data$inputs$x,
                    anno = probe.correction.test.data$inputs$anno,
                    Probe.Correction.Factor = NA
                    );
                },
            regexp = 'parameter argument'
            );
        }
    );

test_that(
    'probe.correction.factor.normalization does not alter input with factor "none"', {
        load('data/probe.correction.Rda');

        result <- probe.correction.factor.normalization(
            x = probe.correction.test.data$inputs$x,
            anno = probe.correction.test.data$inputs$anno,
            Probe.Correction.Factor = 'none'
            );
        expected.result <- list(
            x = probe.correction.test.data$inputs$x,
            anno = probe.correction.test.data$inputs$anno
            );
        expect_equivalent(result, expected.result);
        }
    );

test_that(
    'probe.correction.factor.normalization errors with NA factor', {
        load('data/probe.correction.Rda');
        anno.message <- probe.correction.test.data$inputs$anno;
        anno.message[c(7, 9), 'Name'] <- paste(anno.message[c(7, 9), 'Name'], '(+++ See Message Below)');

        expect_error(
            {
                probe.correction.factor.normalization(
                    x = probe.correction.test.data$inputs$x,
                    anno = anno.message,
                    Probe.Correction.Factor = 'none'
                    );
                },
            regexp = 'probe level background correction'
            );
        }
    );

test_that(
    'probe.correction.factor.normalization errors with NA factor', {
        load('data/probe.correction.Rda');
        anno.message <- probe.correction.test.data$inputs$anno;
        message.i <- c(7, 9);
        anno.message[message.i, 'Name'] <- paste(anno.message[message.i, 'Name'], '(+++ See Message Below)');

        result <- probe.correction.factor.normalization(
            x = probe.correction.test.data$inputs$x,
            anno = anno.message,
            Probe.Correction.Factor = 'filter'
            );
        expected.result <- list(
            x = probe.correction.test.data$inputs$x[-message.i, ],
            anno = probe.correction.test.data$inputs$anno[-message.i, ]
            );
        expect_equivalent(result, expected.result);
        }
    );
