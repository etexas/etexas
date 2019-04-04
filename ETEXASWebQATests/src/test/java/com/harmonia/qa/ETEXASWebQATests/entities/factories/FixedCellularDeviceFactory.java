package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating fixed cellular device objects
 *
 * @author llaroussini
 * @author rsmith
 */
public class FixedCellularDeviceFactory {

    /**
     * Returns a new instance of the fixed cellular device object
     *
     * @return a new fixed cellular device object
     */
    public static FixedCellularDevice getFixedCellularDevice() {
        return instantiateFixedCellularDevice();
    }

    /**
     * Gets a new fixed cellular device which may be random or static
     *
     * @param random whether or not the returned device should have static or
     *        randomized values assigned
     * @return the newly created fixed cellular device according to the
     *         randomization parameter
     */
    public static FixedCellularDevice getFixedCellularDevice(boolean random) {
        if (random) {
            return getRandomFixedCellularDevice();
        }
        else {
            return getStaticFixedCellularDevice();
        }
    }

    /**
     * Gets a fixed cellular device with randomly assigned values
     *
     * @return a new fixed cellular device
     */
    private static FixedCellularDevice getRandomFixedCellularDevice() {
        FixedCellularDevice cellDevice = instantiateFixedCellularDevice();
        cellDevice.setName(RandomStringGenerator.nextString(10, false));
        cellDevice.setMacAddress("1" + RandomStringGenerator.nextNumStringOfLength(8));
        cellDevice.setXCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(999 - 1) + 1));
        cellDevice.setYCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(999 - 1) + 1));
        cellDevice.setZCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(999 - 1) + 1));
        return cellDevice;
    }

    /**
     * Gets a fixed cellular device with a known set of values
     *
     * @return a new fixed cellular device with known values
     */
    private static FixedCellularDevice getStaticFixedCellularDevice() {
        FixedCellularDevice cellDevice = instantiateFixedCellularDevice();
        cellDevice.setName("Test Fixed Cell");
        cellDevice.setMacAddress("123456789");
        cellDevice.setXCoordinate(Integer.toString(500));
        cellDevice.setYCoordinate(Integer.toString(1500));
        cellDevice.setZCoordinate(Integer.toString(2000));
        return cellDevice;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed cellular device object
     */
    private static FixedCellularDevice instantiateFixedCellularDevice() {
        return new FixedCellularDevice();
    }

}
