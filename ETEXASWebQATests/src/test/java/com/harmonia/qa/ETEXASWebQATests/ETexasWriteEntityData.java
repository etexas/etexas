package com.harmonia.qa.ETEXASWebQATests;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasTestBase;
import com.harmonia.qa.Utilities.HarmoniaFileUtils;
import com.harmonia.qa.webdriver.utilities.BasicWebDriverManager;

/**
 * Writes data from the EntityManager to the JSON file
 *
 * @author llaroussini
 */
public class ETexasWriteEntityData extends ETexasTestBase {

    /**
     * Method for writing out data after the tests have run
     *
     * @throws IOException if errors are encountered writing to the file
     */
    @Test
    public void writeData() throws IOException {
        BasicWebDriverManager.get().printTotalWaitTime();
        try {
            File outputFile = new File(ETEXAS_JSON_DATA_FILE);
            HarmoniaFileUtils.clearFileContents(outputFile);
            HarmoniaFileUtils.writeToFile(ETexasEntityManager.getEntities(), outputFile);
        }
        catch (IOException e) {
            e.printStackTrace();
            throw e;
        }
    }

}
