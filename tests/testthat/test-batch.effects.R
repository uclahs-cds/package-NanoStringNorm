test_that(
    'batch.effects regression case', {
        load('data/NanoStringNorm.Rda');

        result <- NanoStringNorm:::get.batch.effects(
            NanoStringNorm.test.data$inputs$x,
            NanoStringNorm.test.data$inputs$anno,
            sample.summary.stats = NanoStringNorm.test.data$inputs$sample.summary,
            traits = NanoStringNorm.test.data$inputs$trait
            );

        expect_equivalent(result, NanoStringNorm.test.data$outputs$batch);
        }
    );
