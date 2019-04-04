package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CellTower;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the create cell tower modal
 *
 * @author llaroussini
 */
public class CreateCellTowerModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public CreateCellTowerModal(WebDriver driver) {
        super(driver);
    }

    //////////////////
    // Enumerations
    /////////////////

    /**
     * Enumeration of Add Cell Tower text boxes
     *
     * @author llaroussini
     */
    public enum CreateCellTowerTextBox {
        /**
         * Cell Tower Provider text box
         */
        CELL_TOWER_PROVIDER("Cell Tower Provider:", "provider"),
        /**
         * X Coordinate text box
         */
        X_COORDINATE("X Coordinate:", "x"),
        /**
         * Y Coordinate text box
         */
        Y_COORDINATE("Y Coordinate:", "y"),
        /**
         * Z Coordinate text box
         */
        Z_COORDINATE("Z Coordinate:", "z");

        /**
         * The label of the text box as appears in the application
         */
        private String label;

        /**
         * The name attribute of the text box as appears in the application
         */
        private String name;

        /**
         * Default constructor
         *
         * @param label The string to set as the label
         * @param name The string to set at the name attribute
         */
        CreateCellTowerTextBox(String label, String name) {
            this.label = label;
            this.name = name;
        }

        /**
         * Gets the label associated with the text box as it is displayed in the
         * Web UI
         *
         * @return The label of the text box
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name attribute associated with the text box as it is
         * displayed in the Web UI
         *
         * @return The name attribute of the text box
         */
        public String getName() {
            return this.name;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the create cell tower header
     */
    private static final String CREATE_CELL_TOWER_HEADER_TEXT = "Create Cell Tower";

    /**
     * The xpath prefix to all text boxes in the Add Cell Tower form
     */
    private static final String TEXT_BOX_XPATH_PREFIX = "//input[@name='";

    /**
     * Xpath prefix to buttons in Create Cell Tower modal
     */
    private static final String CREATE_CELL_TOWER_BTN_XPATH_PREFIX = "//a[contains(@id, 'celltower')][contains(@id, 'Create')]//span[contains(@id, 'button')][text()='";

    /**
     * Error text displayed when invalid cell tower provider name is used
     */
    private static final String INVALID_CELL_TOWER_PROVIDER_TEXT = "Cellular provider name must consist of only letters, numbers, dashes, and spaces. It cannot start or end with a space or dash, and can have at most one consecutive space.";

    /**
     * Text displayed in the create cell tower help header
     */
    private static final String CREATE_CELL_TOWER_HELP_HEADER_TEXT = "Create Cell Tower Help";

    /**
     * Text displayed as create cell tower help content
     */
    private static final String CREATE_CELL_TOWER_HELP_CONTENT_TEXT = "Create a new cellular communications tower with the specified coordinates";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the given button in the add cell tower window
     *
     * @param btn - the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(CREATE_CELL_TOWER_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
    }

    /**
     * Gets the Cell Tower Provider text box
     *
     * @param -the speficic text box to get
     * @return the text box element
     */
    private El getTextBox(CreateCellTowerTextBox textBox) {
        return el(By.xpath(TEXT_BOX_XPATH_PREFIX + textBox.getName() + "']"));
    }

    /**
     * Gets the value displayed in the given text box in the Create Cell Tower modal
     *
     * @param textBox -the specific text box from which to get the value
     * @return the displayed value in the given text box
     */
    private String getTextBoxValue(CreateCellTowerTextBox textBox) {
        return getTextBox(textBox).getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the create cell tower header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateCellTowerHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Checks to see if the create button is displayed
     *
     * @return true if the create button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CREATE));
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.RESET));

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getBtn(BtnNames.CANCEL));

    }

    /**
     * Checks to see if the cell tower provider text box is displayed
     *
     * @param textBox -the text box expected
     * @return true if the text box is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isTextBoxDisplayed(CreateCellTowerTextBox textBox) {
        return isElementDisplayed(getTextBox(textBox));
    }

    /**
     * Checks to see if Cell Tower Provider Whitespace error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCellTowerProviderWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(CreateCellTowerTextBox.CELL_TOWER_PROVIDER.getLabel());
    }

    /**
     * Checks to see if Invalid Cell Tower Provider error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidCellTowerProviderErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_CELL_TOWER_PROVIDER_TEXT);
    }

    /**
     * Checks to see if Invalid X Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidXCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellTowerTextBox.X_COORDINATE.getLabel());
    }

    /**
     * Checks to see if Invalid Y Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidYCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellTowerTextBox.Y_COORDINATE.getLabel());
    }

    /**
     * Checks to see if Invalid Z Coordinate error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidZCoordinateErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(CreateCellTowerTextBox.Z_COORDINATE.getLabel());
    }

    /**
     * Checks to see if the Create Cell Tower help header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCreateCellTowerHelpHeaderDisplayed() {
        return isHeaderDisplayed(CREATE_CELL_TOWER_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the Create Cell Tower help content is displayed
     *
     * @return true if the content text is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isCreateCellTowerHelpContentDisplayed() {
        return isContentDisplayed(CREATE_CELL_TOWER_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in the help window
     *
     * @return true if the ok button is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(CREATE_CELL_TOWER_HELP_HEADER_TEXT);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets provider value from given cell tower in provider text box
     *
     * @param tower -the cell tower
     */
    public void setCellTowerProvider(CellTower tower) {
        String text = tower.getProvider();
        setCellTowerProvider(text);
    }

    /**
     * Sets given text in Cell Tower Provider text box
     *
     * @param text -the text to set
     */
    public void setCellTowerProvider(String text) {
        getTextBox(CreateCellTowerTextBox.CELL_TOWER_PROVIDER).setText(text);
    }

    /**
     * Sets x coordinate from given cell tower in x coordinate text box
     *
     * @param tower -the cell tower
     */
    public void setXCoordinate(CellTower tower) {
        String x = tower.getXCoordinate();
        setXCoordinate(x);
    }

    /**
     * Sets given text in X Coordinate text box
     *
     * @param x -the coordinate to set
     */
    public void setXCoordinate(String x) {
        getTextBox(CreateCellTowerTextBox.X_COORDINATE).setText(x);
    }

    /**
     * Sets y coordinate from given cell tower in y coordinate text box
     *
     * @param tower -the cell tower
     */
    public void setYCoordinate(CellTower tower) {
        String y = tower.getYCoordinate();
        setYCoordinate(y);
    }

    /**
     * Sets given text in Y Coordinate text box
     *
     * @param y -the coordinate to set
     */
    public void setYCoordinate(String y) {
        getTextBox(CreateCellTowerTextBox.Y_COORDINATE).setText(y);
    }

    /**
     * Sets z coordinate from given cell tower in z coordinate text box
     *
     * @param tower -the cell tower
     */
    public void setZCoordinate(CellTower tower) {
        String z = tower.getZCoordinate();
        setZCoordinate(z);
    }

    /**
     * Sets given text in Z Coordinate text box
     *
     * @param z -the coordinate to set
     */
    public void setZCoordinate(String z) {
        getTextBox(CreateCellTowerTextBox.Z_COORDINATE).setText(z);
    }

    /**
     * Click the Create button when error is expected
     */
    public void clickCreateErrorExpected() {
        getBtn(BtnNames.CREATE).click();
    }

    /**
     * Click the Create button
     *
     * @param success -true if success is expected, false if error expected
     */
    public void clickCreate(boolean success) {
        getBtn(BtnNames.CREATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + CREATE_CELL_TOWER_HEADER_TEXT + "')]"));
            getPage(ConfigureCellTowersPartialPage.class);
        }
    }

    /**
     * Click the Create button when errors are expected
     */
    public void clickCreateNoRtrn() {
        getBtn(BtnNames.CREATE).click();
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded Configure Environment Page
     */
    public SimulationSettingsModal clickCancel() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Clicks the Close icon in the Create Cell Tower header
     */
    public void clickCreateCellTowerCloseIcon() {
        clickCloseIcon(CREATE_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(CREATE_CELL_TOWER_HELP_HEADER_TEXT);
    }

    /**
     * Clicks the Create Cell Tower help icon
     */
    public void clickHelpIcon() {
        getHelpIcon(CREATE_CELL_TOWER_HEADER_TEXT).click();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkCreateCellTowerHeaderIcons() {
        checkHeaderIcons(CREATE_CELL_TOWER_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Create Cell Tower Help header not displayed as expected.", isCreateCellTowerHelpHeaderDisplayed());
        Assert.assertTrue("Create Cell Tower Help content not displayed as expected.", isCreateCellTowerHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Create, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Create button was not found.", isCreateBtnDisplayed());
        Assert.assertTrue("The Reset button was not found.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button was not found.", isCancelBtnDisplayed());
    }

    /**
     * Checks for presence of Cell Tower Provider, X Coordinate, Y Coordinate,
     * and Z Coordinate text boxes
     */
    public void checkFieldsDisplayed() {
        Assert.assertTrue("Cell Tower Provider text box was not found.", isTextBoxDisplayed(CreateCellTowerTextBox.CELL_TOWER_PROVIDER));
        Assert.assertTrue("X Coordinate text box was not found.", isTextBoxDisplayed(CreateCellTowerTextBox.X_COORDINATE));
        Assert.assertTrue("Y Coordinate text box was not found.", isTextBoxDisplayed(CreateCellTowerTextBox.Y_COORDINATE));
        Assert.assertTrue("Z Coordinate text box was not found.", isTextBoxDisplayed(CreateCellTowerTextBox.Z_COORDINATE));
    }

    /**
     * Sets values from given cellular tower into Cell Tower Provider, X
     * Coordinate, Y Coordinate, and Z Coordinate text boxes
     *
     * @param tower -the cell tower to set
     */
    public void setAllFields(CellTower tower) {
        setCellTowerProvider(tower);
        setXCoordinate(tower);
        setYCoordinate(tower);
        setZCoordinate(tower);
    }

    /**
     * Sets all given values into Cell Tower Provider, X Coordinate, Y
     * Coordinate, and Z Coordinate text boxes
     *
     * @param provider -the cell tower provider
     * @param x -the x coordinate
     * @param y -the y coordinate
     * @param z -the z coordinate
     */
    public void setAllFields(String provider, String x, String y, String z) {
        setCellTowerProvider(provider);
        setXCoordinate(x);
        setYCoordinate(y);
        setZCoordinate(z);
    }

    /**
     * Checks values displayed in all fields and verifies they match the values
     * for the given cell tower
     *
     * @param tower -the tower expected
     */
    public void checkFieldValues(CellTower tower) {
        String provider = tower.getProvider();
        String x = tower.getXCoordinate();
        String y = tower.getYCoordinate();
        String z = tower.getZCoordinate();
        checkFieldValues(provider, x, y, z);

    }

    /**
     * Checks values displayed in all fields and verifies they match the given
     * string values
     *
     * @param provider -the provider expected
     * @param x -the x coordinate expected
     * @param y -the y coordinate expected
     * @param z -the z coordinate expected
     */
    public void checkFieldValues(String provider, String x, String y, String z) {
        Assert.assertEquals("The cell tower provider value," + provider + ", is not displayed as expected.", provider, getTextBoxValue(CreateCellTowerTextBox.CELL_TOWER_PROVIDER));
        Assert.assertEquals("The x coordinate value," + x + ", is not displayed as expected.", x, getTextBoxValue(CreateCellTowerTextBox.X_COORDINATE));
        Assert.assertEquals("The y coordinate value," + y + ", is not displayed as expected.", y, getTextBoxValue(CreateCellTowerTextBox.Y_COORDINATE));
        Assert.assertEquals("The z coordinate value," + z + ", is not displayed as expected.", z, getTextBoxValue(CreateCellTowerTextBox.Z_COORDINATE));
    }

    /**
     * Verifies 'required field' error displays for all fields (checks Cell
     * Tower Provider, X Coordinate, Y Coordinate, and Z Coordinate text boxes)
     */
    public void checkRequiredFieldErrorAllFields() {
        Assert.assertTrue("Required field error is not displayed for the Cell Tower Provider field.", isFieldRequiredErrorDisplayed(CreateCellTowerTextBox.CELL_TOWER_PROVIDER.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the X Coordinate field.", isFieldRequiredErrorDisplayed(CreateCellTowerTextBox.X_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Y Coordinate field.", isFieldRequiredErrorDisplayed(CreateCellTowerTextBox.Y_COORDINATE.getLabel()));
        Assert.assertTrue("Required field error is not displayed for the Z Coordinate field.", isFieldRequiredErrorDisplayed(CreateCellTowerTextBox.Z_COORDINATE.getLabel()));
    }
}
