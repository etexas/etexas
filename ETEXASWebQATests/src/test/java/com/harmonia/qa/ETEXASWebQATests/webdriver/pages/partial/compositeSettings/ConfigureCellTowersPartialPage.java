package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal.ConfigureBtn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the configuration of cellular towers partial page
 *
 * @author llaroussini
 */
public class ConfigureCellTowersPartialPage extends SimulationSettingsModal {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public ConfigureCellTowersPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////
    // Enumerations
    ///////////////

    /**
     * Enumeration of columns in cell towers table
     *
     * @author llaroussini
     */
    public enum CellTowerTableColumnHeader {
        /**
         * ID column header
         */
        ID("ID"),
        /**
         * Provider column header
         */
        PROVIDER("Provider"),
        /**
         * X Coordinate column header
         */
        X_COORDINATE("X (cm)"),
        /**
         * Y Coordinate column header
         */
        Y_COORDINATE("Y (cm)"),
        /**
         * Z Coordinate column header
         */
        Z_COORDINATE("Z (cm)");

        /**
         * The label of the column header as appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CellTowerTableColumnHeader(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the column header as it is displayed
         * in the Web UI
         *
         * @return The label of the column header
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath prefix to cell towers table column
     */
    private static final String CELL_TOWERS_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'cell')][contains(@id, 'tower')]//span[@class='x-column-header-text-inner'][text()='";

    /**
     * Xpath prefix to buttons
     */
    private static final String CONFIGURE_TOWER_BTN_XPATH_PREFIX = "//div[contains(@id, 'cell')][contains(@id, 'tower')]//span[contains(@id, 'button')][text()='";

    /**
     * Xpath to the ID cell contained in a row
     */
    private static final String ID_CELL_XPATH = "./td[contains(@class, 'x-grid-cell-first')]//div";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given column header element
     *
     * @param column -the column to get
     * @return the column header element
     */
    private El getCellTowerColumnHeader(CellTowerTableColumnHeader column) {
        return el(By.xpath(CELL_TOWERS_TABLE_COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button
     */
    private El getCellTowerBtn(ConfigureBtn btn) {
        return el(By.xpath(CONFIGURE_TOWER_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the row associated with the given cell tower
     *
     * @param provider -the provider associated with the cell tower
     * @return the row element
     */
    private El getCellTowerRow(String provider) {
        El cell = el(getCell(provider));
        return cell.el(By.xpath("./ancestor::tr"));
    }

    /**
     * Gets the ID associated with the given cell tower
     *
     * @param cellTower -the cell tower
     * @return the ID as string
     */
    public String getCellTowerID(CellTower cellTower) {
        El row = getCellTowerRow(cellTower.getProvider());
        El idCell = row.el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the column header expected is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader column) {
        return isElementDisplayed(getCellTowerColumnHeader(column));
    }

    /**
     * Checks to see if the cell tower create button is displayed
     *
     * @return true if cell tower create button is displayed, false if it is not
     *         or cannot be found
     */
    public boolean isCellTowerCreateBtnDisplayed() {
        return isElementDisplayed(getCellTowerBtn(ConfigureBtn.CREATE));
    }

    /**
     * Checks to see if the cell tower edit button is displayed
     *
     * @return true if the cell tower edit button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isCellTowerEditBtnDisplayed() {
        return isElementDisplayed(getCellTowerBtn(ConfigureBtn.EDIT));
    }

    /**
     * Checks to see if the cell tower delete button is displayed
     *
     * @return true if the cell tower delete button is displayed, false if it is
     *         not or cannot be found
     */
    public boolean isCellTowerDeleteBtnDisplayed() {
        return isElementDisplayed(getCellTowerBtn(ConfigureBtn.DELETE));
    }

    /**
     * Checks to see if the cell tower create button is enabled
     *
     * @return true if the cell tower create button is enabled, false if it is
     *         disabled
     */
    public boolean isCellTowerCreateBtnEnabled() {
        return getCellTowerBtn(ConfigureBtn.CREATE).isEnabled();
    }

    /**
     * Checks to see if the cell tower edit button is enabled
     *
     * @return true if the cell tower edit button is enabled, false if it is
     *         disabled
     */
    public boolean isCellTowerEditBtnEnabled() {
        return getCellTowerBtn(ConfigureBtn.EDIT).isEnabled();
    }

    /**
     * Checks to see if the cell tower delete button is enabled
     *
     * @return true if the cell tower delete button is enabled, false if it is
     *         disabled
     */
    public boolean isCellTowerDeleteBtnEnabled() {
        return getCellTowerBtn(ConfigureBtn.DELETE).isEnabled();
    }

    /**
     * Checks to see if the given tower attribute is displayed in the table
     *
     * @param attirbute -the attribute associated with the tower
     * @return true if displayed, false if not
     */
    public boolean isCellTowerDisplayed(String attibute) {
        return isElementDisplayed(el(getCell(attibute)));
    }

    /**
     * Checks if the row for the given cell tower is selected
     *
     * @param tower the cell tower to check
     * @return true if selected, false if de-selected
     */
    public boolean isCellTowerRowSelected(CellTower tower) {
        String id = tower.getID();
        return isRowSelected(id);
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Create button
     *
     * @return the newly loaded Create Cell Tower Form
     */
    public CreateCellTowerModal clickCreateCellTowerBtn() {
        getCellTowerBtn(ConfigureBtn.CREATE).click();
        return getPage(CreateCellTowerModal.class);
    }

    /**
     * Clicks the Delete button
     *
     * @return the Delete Warning Form
     */
    public ConfirmDeleteModal clickDeleteCellTowerBtn() {
        getCellTowerBtn(ConfigureBtn.DELETE).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit Cell Tower form
     */
    public EditCellTowerForm clickEdit() {
        getCellTowerBtn(ConfigureBtn.EDIT).click();
        return getPage(EditCellTowerForm.class);
    }

    /////////////
    // Utilities
    /////////////

    /**
     * Verifies all expected buttons are displayed (Create, Edit, and Delete)
     */
    public void checkBtns() {
        Assert.assertTrue("Create button not displayed as expected.", isCellTowerCreateBtnDisplayed());
        Assert.assertTrue("Edit button not displayed as expected.", isCellTowerEditBtnDisplayed());
        Assert.assertTrue("Delete button not displayed as expected.", isCellTowerDeleteBtnDisplayed());
    }

    /**
     * Verifies all column headers are displayed in cell towers table (ID,
     * Provider, X, Y, and Z)
     */
    public void checkCellTowersColumnHeaders() {
        Assert.assertTrue("The ID column header not displayed as expected in Cell Towers table.", isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader.ID));
        Assert.assertTrue("The Provider column header not displayed as expected in Cell Towers table.", isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader.PROVIDER));
        Assert.assertTrue("The X (cm) column header not displayed as expected in Cell Towers table.", isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader.X_COORDINATE));
        Assert.assertTrue("The Y (cm) column header not displayed as expected in Cell Towers table.", isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader.Y_COORDINATE));
        Assert.assertTrue("The Z (cm) column header not displayed as expected in Cell Towers table.", isCellTowerColumnHeaderDisplayed(CellTowerTableColumnHeader.Z_COORDINATE));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(CELL_TOWERS_TABLE_COLUMN_XPATH_PREFIX + CellTowerTableColumnHeader.PROVIDER.label + "']"), 10);
    }

}
