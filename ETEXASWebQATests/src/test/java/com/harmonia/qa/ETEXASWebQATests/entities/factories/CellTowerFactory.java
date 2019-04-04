package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating cell tower objects
 *
 * @author llaroussini
 */
public class CellTowerFactory {

    /**
     * Returns a new instance of the cell tower object
     *
     * @return a new cell tower object
     */
    public static CellTower getCellTower() {
        return instantiateCellTower();
    }

    /**
     * Gets a new cell tower which may be random or static
     *
     * @param random whether or not the returned tower should have static or
     *        randomized values assigned
     * @return the newly created cell tower according to the randomization
     *         parameter
     */
    public static CellTower getCellTower(boolean random) {
        if (random) {
            return getRandomCellTower();
        }
        else {
            return getStaticCellTower();
        }
    }

    /**
     * Gets a cell tower with randomly assigned values
     *
     * @return a new cell tower
     */
    private static CellTower getRandomCellTower() {
        CellTower cellTower = instantiateCellTower();
        cellTower.setProvider(RandomStringGenerator.nextLetterString(10));
        cellTower.setXCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(5000 - 1) + 1));
        cellTower.setYCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(5000 - 1) + 1));
        cellTower.setZCoordinate(Integer.toString(RandomNumberGenerator.nextInteger(5000 - 1) + 1));
        return cellTower;
    }

    /**
     * Gets a cell tower with a known set of values
     *
     * @return a new cell tower with known values
     */
    private static CellTower getStaticCellTower() {
        CellTower cellTower = instantiateCellTower();
        cellTower.setProvider("Test Provider");
        cellTower.setXCoordinate("100");
        cellTower.setYCoordinate("250");
        cellTower.setZCoordinate("1500");
        return cellTower;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed cell tower object
     */
    private static CellTower instantiateCellTower() {
        return new CellTower();
    }

}