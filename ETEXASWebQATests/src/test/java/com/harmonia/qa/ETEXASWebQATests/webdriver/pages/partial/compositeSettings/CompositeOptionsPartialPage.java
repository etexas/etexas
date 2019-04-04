package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.webdriver.utilities.elements.El;


/**
 * Page class representing the Composite Options tab in the Composite Settings
 * Modal
 *
 * @author llaroussini
 */
public class CompositeOptionsPartialPage extends CompositeSettingsModal {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public CompositeOptionsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////////
    // Enumerations
    ///////////////

    /**
     * Enumeration of display field IDs
     *
     * @author llaroussini
     */
    public enum CoordinatesDisplayField {
        /**
         * Latitude Display Field
         */
        LATITUDE("latitdue"),
        /**
         * Longitude Display Field
         */
        LONGITUDE("longitude"),
        /**
         * Geographic Calculator Display Field
         */
        GEO_CALCULATOR("geographic");

        /**
         * The id associated with the display field
         */
        private String id;

        /**
         * Default constructor; sets the ID
         *
         * @param id The string to set as the ID
         */
        CoordinatesDisplayField(String id) {
            this.id = id;
        }

        /**
         * Gets the ID associated with the display field
         *
         * @return The ID of the display field
         */
        public String getID() {
            return this.id;
        }
    }

    /**
     * Enumeration of buttons
     *
     * @author llaroussini
     */
    public enum CompositeOptionsBtn {
        /**
         * Edit button
         */
        EDIT("Edit"),
        /**
         * Cellular Options button
         */
        CELL_OPTIONS("Cellular Options");

        /**
         * The label associated with the button
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CompositeOptionsBtn(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the button
         *
         * @return The label of the button
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Xpath to the Coordinates section header
     */
    private static final String COORDINATES_SECTION_HEADER_XPATH = "//div[contains(@id, 'coordinates')][text()='Coordinates']";

    /**
     * Xpath to the Communications section header
     */
    private static final String COMMUNICATIONS_SECTION_HEADER_XPATH = "//div[contains(@id, 'communications')][text()='Communications']";

    /**
     * Xpath prefix to elements in modal
     */
    private static final String OPTIONS_ELEMENT_XPATH_PREFIX = "//div[contains(@id, 'composite')][contains(@id, 'options')]";

    /**
     * Xpath prefix to buttons in modal
     */
    private static final String BTN_XPATH_PREFIX = OPTIONS_ELEMENT_XPATH_PREFIX + "//a//span[text()='";

    /**
     * Xpath suffix to buttons in modal
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath prefix to the display fields in the Coordinates section
     */
    private static final String COORDINATES_DISPLAY_FIELD_XPATH_PREFIX = OPTIONS_ELEMENT_XPATH_PREFIX + "//div[contains(@id, 'coordinates')]//div[@role='textbox'][contains(@id, '";

    /**
     * Xpath prefix to the display fields in the Communications section
     */
    private static final String COMMUNICATIONS_DISPLAY_FIELD_XPATH = OPTIONS_ELEMENT_XPATH_PREFIX + "//div[contains(@id, 'communication')]//div[@role='textbox'][contains(@id, 'model')]";

    /**
     * Xpath to warning text displayed in Communications section
     */
    private static final String COMM_MODEL_WARNING_XPATH = OPTIONS_ELEMENT_XPATH_PREFIX
            + "//div[text()='NOTE: cellular options may be configured at any time, but will only be in effect during executions when a cellular communications model (NS3) is selected']";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the Coordinates section header
     *
     * @return the header element
     */
    private El getCoordinatesSectionHeader() {
        return el(By.xpath(COORDINATES_SECTION_HEADER_XPATH));
    }

    /**
     * Gets the Communications section header
     *
     * @return the header element
     */
    private El getCommunicationsSectionHeader() {
        return el(By.xpath(COMMUNICATIONS_SECTION_HEADER_XPATH));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button in the Composite Options tab to get
     * @return the button element
     */
    private El getBtn(CompositeOptionsBtn btn) {
        return el(By.xpath(BTN_XPATH_PREFIX + btn.getLabel() + BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the given display field in the Coordinates section
     *
     * @param field -the display field in the Coordinates section to get
     * @return the field element
     */
    private El getCoordinatesDisplayedField(CoordinatesDisplayField field) {
        return el(By.xpath(COORDINATES_DISPLAY_FIELD_XPATH_PREFIX + field.getID() + "')]"));
    }

    /**
     * Gets the Communications modal display field
     *
     * @return the display field element
     */
    private El getCommModelDisplayField() {
        return el(By.xpath(COMMUNICATIONS_DISPLAY_FIELD_XPATH));
    }

    /**
     * Gets the Communications Model warning area
     *
     * @return the warning area element
     */
    private El getCommModelWarning() {
        return el(By.xpath(COMM_MODEL_WARNING_XPATH));
    }

    /**
     * Gets the displayed Latitude value
     *
     * @return the displayed Latitude value as Double
     */
    public Double getDisplayedLatitude() {
        String lat = getCoordinatesDisplayedField(CoordinatesDisplayField.LATITUDE).getText();
        return Double.parseDouble(lat);
    }

    /**
     * Gets the displayed Longitude value
     *
     * @return the displayed Longitude value as Double
     */
    public Double getDisplayedLongitude() {
        String longitude = getCoordinatesDisplayedField(CoordinatesDisplayField.LONGITUDE).getText();
        return Double.parseDouble(longitude);
    }

    /**
     * Gets the displayed Geographic Calculator value
     *
     * @return the displayed Geographic Calculator value
     */
    public String getDisplayedGeoCalculator() {
        return getCoordinatesDisplayedField(CoordinatesDisplayField.GEO_CALCULATOR).getText();
    }

    /**
     * Gets the displayed Communications Model value
     *
     * @return the displayed Communications Model value
     */
    public String getDisplayedCommModel() {
        return getCommModelDisplayField().getText();
    }

    ////////////
    // Checkers
    ////////////
    /**
     * Checks if the Coordinates section header is displayed
     *
     * @return true if the Coordinates section header is displayed, false
     *         otherwise
     */
    public boolean isCoordinatesSectionHeaderDisplayed() {
        return isElementDisplayed(getCoordinatesSectionHeader());
    }

    /**
     * Checks if the Communications section header is displayed
     *
     * @return true if the Communications section header is displayed, false
     *         otherwise
     */
    public boolean isCommunicationsSectionHeaderDisplayed() {
        return isElementDisplayed(getCommunicationsSectionHeader());
    }

    /**
     * Checks if the given button is displayed
     *
     * @param btn -the button in the Composite Options tab expected
     * @return true if the given button is displayed, false otherwise
     */
    public boolean isBtnDisplayed(CompositeOptionsBtn btn) {
        return isElementDisplayed(getBtn(btn));
    }

    /**
     * Checks if the given button is disabled
     *
     * @param btn -the button in the Composite Options tab expected
     * @return true if the given button is disabled, false if enabled
     */
    public boolean isBtnDisabled(CompositeOptionsBtn btn) {
        El el = getBtn(btn);
        String disabled = el.getAttribute(ARIA_DISABLED_ATTRIBUTE);
        return "true".equals(disabled);
    }

    /**
     * Checks if the given display field in Coordinates section is displayed
     *
     * @param field -the display field in the Coordinates section expected
     * @return true if the display field is displayed, false otherwise
     */
    public boolean isCoordinatesDisplayedFieldDisplayed(CoordinatesDisplayField field) {
        return isElementDisplayed(getCoordinatesDisplayedField(field));
    }

    /**
     * Checks if the Communications modal display field is displayed
     *
     * @return true if the Communications modal display field is displayed,
     *         false otherwise
     */
    public boolean isCommModelDisplayFieldDisplayed() {
        return isElementDisplayed(getCommModelDisplayField());
    }

    /**
     * Checks if the Communications Model warning area is displayed
     *
     * @return true if the Communications Model warning area is displayed, false
     *         otherwise
     */
    public boolean isCommModelWarningDisplayed() {
        return isElementDisplayed(getCommModelWarning());
    }

    ///////////////
    // Verification
    ///////////////

    /**
     * Verifies the Coordinates section header is displayed
     */
    public void checkCoordinatesSectionHeaderIsDisplayed() {
        Assert.assertTrue("Coordinates section header is not displayed as expected.", isCoordinatesSectionHeaderDisplayed());
    }

    /**
     * Verifies the Communications section header is displayed
     */
    public void checkCommunicationsSectionHeaderIsDisplayed() {
        Assert.assertTrue("Communications section header is not displayed as expected.", isCommunicationsSectionHeaderDisplayed());
    }

    /**
     * Verifies the Edit button is displayed
     */
    public void checkEditBtnIsDisplayed() {
        CompositeOptionsBtn editBtn = CompositeOptionsBtn.EDIT;
        Assert.assertTrue("The " + editBtn.getLabel() + " button is not displayed as expected.", isBtnDisplayed(editBtn));
    }

    /**
     * Verifies the Cellular Options button is displayed
     */
    public void checkCellOptionsBtnIsDisplayed() {
        CompositeOptionsBtn cellOptionsBtn = CompositeOptionsBtn.CELL_OPTIONS;
        Assert.assertTrue("The " + cellOptionsBtn.getLabel() + " button is not displayed as expected.", isBtnDisplayed(cellOptionsBtn));
    }

    /**
     * Verifies the given display field in Coordinates section is displayed
     *
     * @param field -the display field in the Coordinates section expected
     */
    public void checkCoordinatesDisplayFieldIsDisplayed(CoordinatesDisplayField field) {
        Assert.assertTrue("The " + field.getID() + " display field is not displayed as expected.", isCoordinatesDisplayedFieldDisplayed(field));
    }

    /**
     * Verifies the Communications modal display field is displayed
     */
    public void checkCommModelDisplayFieldIsDisplayed() {
        Assert.assertTrue("Communications modal display field is not displayed as expected.", isCommModelDisplayFieldDisplayed());
    }

    /**
     * Verifies the Communications Model warning area is displayed
     */
    public void checkCommModelWarningIsDisplayed() {
        Assert.assertTrue("Communications Model warning area is not displayed as expected.", isCommModelWarningDisplayed());
    }

    ////////////////
    // Interaction
    ////////////////

    /**
     * Clicks the Edit button
     *
     * @return the newly loaded Edit Options Form
     */
    public EditOptionsForm clickEdit() {
        getBtn(CompositeOptionsBtn.EDIT).click();
        return getPage(EditOptionsForm.class);
    }

    /**
     * Clicks the Cellular Options button
     *
     * @return the Edit Cellular Options modal
     */
    public EditCellularOptionsModal clickCellularOptions() {
        getBtn(CompositeOptionsBtn.CELL_OPTIONS).click();
        return getPage(EditCellularOptionsModal.class);
    }

    //////////////
    // Utilities
    /////////////

    /**
     * Verifies both the Coordinates section header and Communications section
     * header are displayed
     */
    public void checkSections() {
        checkCoordinatesSectionHeaderIsDisplayed();
        checkCommunicationsSectionHeaderIsDisplayed();
    }

    /**
     * Verifies Latitude, Longitude, and Geographic Calculator display fields
     * are displayed in the Coordinates section
     */
    public void checkCoordinatesDisplayFields() {
        checkCoordinatesDisplayFieldIsDisplayed(CoordinatesDisplayField.LATITUDE);
        checkCoordinatesDisplayFieldIsDisplayed(CoordinatesDisplayField.LONGITUDE);
        checkCoordinatesDisplayFieldIsDisplayed(CoordinatesDisplayField.GEO_CALCULATOR);
    }

    /**
     * Verifies the given values match the displayed values in the Edit Options
     * fields (Latitude, Longitude, Geographic Calculator, and Communications
     * Model)
     *
     * @param latitude -the latitude value expected
     * @param longitude -the longitude value expected
     * @param geoCalc -the geographic calculator option expected
     * @param commModel -the communications model option expected
     */
    public void checkValues(Double latitude, Double longitude, String geoCalc, String commModel) {
        Assert.assertEquals("The Latitude value of " + latitude + " is not displayed as expected.", latitude, getDisplayedLatitude());
        Assert.assertEquals("The Longitude value of" + longitude + " is not displayed as expected.", longitude, getDisplayedLongitude());
        Assert.assertEquals("The Geographic Calculator value is not displayed as expected.", geoCalc, getDisplayedGeoCalculator());
        Assert.assertEquals("The Communications Model value is not displayed as expected.", commModel, getDisplayedCommModel());
    }

    /**
     * Verifies the given values match the displayed values in the Edit Options
     * fields (Latitude, Longitude, Geographic Calculator, and Communications
     * Model)
     *
     * @param latitude -the latitude value expected
     * @param longitude -the longitude value expected
     * @param geoCalc -the geographic calculator option expected
     * @param commModel -the communications model option expected
     */
    public void checkValues(String latitude, String longitude, String geoCalc, String commModel) {
        Double latDouble = Double.parseDouble(latitude);
        Double longDouble = Double.parseDouble(longitude);
        checkValues(latDouble, longDouble, geoCalc, commModel);
    }

}
