package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularOptionsConfiguration;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditCellularOptionsModal.BandwidthValues;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Entity factory for generating Cellular Options configuration entities
 *
 * @author llaroussini
 */
public class CellularOptionsConfigurationFactory {

    /**
     * Gets a new Advanced Cellular Options Configuration which may be returned
     * random or static
     *
     * @param random whether or not the returned configuration should have
     *        static or randomized values assigned
     * @return the newly created configuration according to the randomization
     *         parameter
     */
    public static CellularOptionsConfiguration getAdvancedCellularConfiguration(boolean random) {
        if (random) {
            return getRandomAdvancedCellularConfiguration();
        }
        else {
            return getStaticAdvancedCellularConfiguration();
        }
    }

    /**
     * Gets an Advanced Cellular Configuration with randomly assigned values
     *
     * @return a new Advanced Cellular Configuration
     */
    private static CellularOptionsConfiguration getRandomAdvancedCellularConfiguration() {
        CellularOptionsConfiguration config = new CellularOptionsConfiguration();
        BandwidthValues uplinkBandwidth = BandwidthValues.values()[RandomNumberGenerator.nextInteger(BandwidthValues.values().length)];
        config.setUplinkBandwith(uplinkBandwidth);
        int uplinkFreqRange = RandomNumberGenerator.nextInteger(24599 - 18000) + 18000;
        config.setUplinkCarrierFrequency(uplinkFreqRange);
        BandwidthValues downlinkBandwidth = BandwidthValues.values()[RandomNumberGenerator.nextInteger(BandwidthValues.values().length)];
        config.setDownlinkBandwith(downlinkBandwidth);
        config.setDownlinkCarrierFrequency(RandomNumberGenerator.nextInteger(6599));
        int cellPowerRange = RandomNumberGenerator.nextInteger(200 + 200) - 200;
        int cellNoiseRange = RandomNumberGenerator.nextInteger(194);
        config.setCellPower(cellPowerRange);
        config.setCellNoise(cellNoiseRange);
        config.setCellTowerPower(cellPowerRange);
        config.setCellTowerNoise(cellNoiseRange);
        return config;
    }

    /**
     * Gets an Advanced Cellular Configuration with a known set of values
     *
     * @return a new Advanced Cellular Configuration with known values
     */
    private static CellularOptionsConfiguration getStaticAdvancedCellularConfiguration() {
        CellularOptionsConfiguration config = new CellularOptionsConfiguration();
        config.setUplinkBandwith(BandwidthValues.ONE_HUNDRED);
        config.setUplinkCarrierFrequency(22456);
        config.setDownlinkBandwith(BandwidthValues.SEVENTY_FIVE);
        config.setDownlinkCarrierFrequency(1500);
        config.setCellPower(-180);
        config.setCellNoise(15);
        config.setCellTowerPower(75);
        config.setCellTowerNoise(100);
        return config;
    }
}
