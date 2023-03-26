module Stats where
    import MathUtils

    avg :: [Double] -> Double
    avg dataset = (summation dataset) / fromIntegral (length dataset)
    
    sampleStdev :: [Double] -> Double
    sampleStdev dataset = sqrt (summation [pow (distance mean num) 2 | num <- dataset] / (fromIntegral (length dataset) - 1))
        where 
            mean = avg dataset
    
    populationStdev :: [Double] -> Double
    populationStdev dataset = sqrt (summation [pow (distance mean num) 2 | num <- dataset] / fromIntegral (length dataset))
        where
            mean = avg dataset

    describe :: [Double] -> [([Char], Double)]
    describe dataset = [
        ("Mean", (avg dataset)), 
        ("Sample Standard Deviation", (sampleStdev dataset)), 
        ("Population Standard Deviation", (populationStdev dataset))]

    extractValuesFromTable :: [(Double, Int)] -> [Double]
    extractValuesFromTable freqTable = [num * fromIntegral freq | (num, freq) <- freqTable]

    avgFreq :: [(Double, Int)] -> Double
    avgFreq dataset = summation values / fromIntegral (length values)
        where 
            values = extractValuesFromTable dataset

    sampleStdevFreq :: [(Double, Int)] -> Double
    sampleStdevFreq dataset = sqrt (summation [pow (distance mean num) 2 | num <- values] / (fromIntegral (length values) - 1))
        where 
            mean = avgFreq dataset
            values = extractValuesFromTable dataset

    populationStdevFreq :: [(Double, Int)] -> Double
    populationStdevFreq dataset = sqrt (summation [pow (distance mean num) 2 | num <- values] / fromIntegral (length values))
        where
            mean = avgFreq dataset
            values = extractValuesFromTable dataset

    describeFreq :: [(Double, Int)] -> [([Char], Double)]
    describeFreq dataset = [
        ("Mean", (avgFreq dataset)),
        ("Sample Standard Deviation", (sampleStdevFreq dataset)),
        ("Population Standard Deviation", (populationStdevFreq dataset))]

