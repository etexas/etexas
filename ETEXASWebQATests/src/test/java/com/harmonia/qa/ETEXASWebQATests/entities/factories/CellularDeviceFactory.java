package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import java.util.concurrent.ThreadLocalRandom;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating Cellular device objects
 *
 * @author llaroussini
 */
public class CellularDeviceFactory {

    /**
     * Returns a new instance of the cellular device object
     *
     * @return a new cellular device object
     */
    public static CellularDevice getCellularDevice() {
        return instantiateCellularDevice();
    }

    /**
     * Gets a new cellular device which may be random or static
     *
     * @param random whether or not the returned device should have static or
     *        randomized values assigned
     * @return the newly created cellular device according to the randomization
     *         parameter
     */
    public static CellularDevice getCellularDevice(boolean random) {
        if (random) {
            return getRandomCellularDevice();
        }
        else {
            return getStaticCellularDevice();
        }
    }

    /**
     * Gets a cellular device with randomly assigned values
     *
     * @return a new cellular device
     */
    private static CellularDevice getRandomCellularDevice() {
        CellularDevice cellDevice = instantiateCellularDevice();
        cellDevice.setName(RandomStringGenerator.nextLetterString(15));
        cellDevice.setCellularPercent(Integer.toString(RandomNumberGenerator.nextInteger(100)));
        cellDevice.setMinCellsPerVehicle(Integer.toString((RandomNumberGenerator.nextInteger(8))));
        cellDevice.setMaxCellsPerVehicle(Integer.toString(ThreadLocalRandom.current().nextInt(Integer.parseInt(cellDevice.getMinCellsPerVehicle()), 8))); //TODO add method to automation base
        return cellDevice;
    }

    /**
     * Gets a cellular device with a known set of values
     *
     * @return a new cellular device with known values
     */
    private static CellularDevice getStaticCellularDevice() {
        CellularDevice cellDevice = instantiateCellularDevice();
        cellDevice.setName("Test Cell");
        cellDevice.setCellularPercent("100");
        cellDevice.setMinCellsPerVehicle("8");
        cellDevice.setMaxCellsPerVehicle("8");
        return cellDevice;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed cellular device object
     */
    private static CellularDevice instantiateCellularDevice() {
        return new CellularDevice();
    }

}