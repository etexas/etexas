package com.harmonia.qa.ETEXASWebQATests.utilities;

/**
 * Properties used throughout eTexas Automated Tests
 *
 * @author cbulloss
 */
public interface ETexasProperties {

    /**
     * Holds the start time for the test suite
     */
    final long START_TIME = System.currentTimeMillis();

    /**
     * The URL where the landing page is accessible
     */
    final String LANDING_PAGE_URL = ETexasPropertiesManager.getString("url");

    /**
     * JSON file where entity data is stored
     */
    final String ETEXAS_JSON_DATA_FILE = ETexasPropertiesManager.getString("json_data_file");

    /**
     * Name of default admin username
     */
    final String defaultAdminUsername = ETexasPropertiesManager.getString("defaultAdminUsername");

    /**
     * Name of default admin password
     */
    final String defaultAdminPassword = ETexasPropertiesManager.getString("defaultAdminPassword");

    /**
     * Name of default EX_05 simulation
     */
    final String defaultEx05Sim = ETexasPropertiesManager.getString("defaultEx05Sim");

    /**
     * Name of default report device
     */
    final String defaultReportDevice = ETexasPropertiesManager.getString("defaultReportDevice");

    /**
     * Host name for the eTEXAS test server
     */
    final String hostName = ETexasPropertiesManager.getString("etexasHostName");

}
