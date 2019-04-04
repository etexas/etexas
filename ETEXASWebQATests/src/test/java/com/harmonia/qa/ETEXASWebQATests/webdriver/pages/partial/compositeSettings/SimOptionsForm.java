package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.ConfigureEnvironmentHelpForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Simulation Options form
 *
 * @author cbulloss
 * @author llaroussini
 */
public class SimOptionsForm extends SimulationSettingsModal {

    ///////////
    // Enumerated elements
    ///////////

    /**
     * Fieldsets displayed in the window
     *
     * @author cbulloss
     */
    public enum Fieldsets {
        /**
         * The Simulation Center fieldset
         */
        SIMULATION_CENTER("Simulation Center"),
        /**
         * The Communications Model fieldset
         */
        COMMUNICATIONS_MODEL("Communications Model"),
        /**
         * The Coordinate Conversion fieldset
         */
        COORDINATE_CONVERSION("Coordinate Conversion"),
        /**
         * The RSE Coverage fieldset
         */
        RSE_COVERAGE("RSE Coverage");

        /**
         * Prefix to the xpath locator used to locate enumerated fieldsets.
         */
        private static final String XPATH_LOCATOR_PREFIX = ".//div[contains(@id,'fieldset')][contains(text(),'";

        /**
         * Suffix to the xpath locator used to locate enumerated fieldsets
         */
        private static final String XPATH_LOCATOR_SUFFIX = "')]";

        /**
         * The label for this fieldset as it appears in the uI
         */
        private String label;

        /**
         * Constructor for this enum
         *
         * @param label the label for this fieldset in the UI
         */
        private Fieldsets(String label) {
            this.label = label;
        }

        /**
         * Gets the label for this fieldset as it appears in the UI
         *
         * @return the label for this fieldset
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets a by locator suitable for locating this fieldset
         *
         * @return a By locator suitable for locating this fieldset
         */
        public By getLocator() {
            return By.xpath(XPATH_LOCATOR_PREFIX + this.getLabel() + XPATH_LOCATOR_SUFFIX);
        }
    }

    /**
     * Model types
     *
     * @author llaroussini
     */
    public enum ModelType {
        /**
         * Idealized model type
         */
        IDEALIZED("Idealized"),
        /**
         * NS3 model type
         */
        NS3("NS3");

        /**
         * The label for this model type as it appears in the uI
         */
        private String label;

        /**
         * Constructor for this enum
         *
         * @param label the label for this model type in the UI
         */
        private ModelType(String label) {
            this.label = label;
        }

        /**
         * Gets the label for this model type as it appears in the UI
         *
         * @return the label for this model type
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Coordinate systems
     *
     * @author llaroussini
     */
    public enum CoordinateSystem {
        /**
         * Geodetic 2D coordinate system
         */
        GEODETIC_2D("Geodetic 2D"),
        /**
         * Spherical coordinate system
         */
        SPHERICAL("Spherical"),
        /**
         * Geodetic 3D coordinate system
         */
        GEODETIC_3D("Geodetic 3D"),
        /**
         * Cartesian coordinate system
         */
        CARTESIAN("Cartesian");

        /**
         * The label for this coordinate system as it appears in the uI
         */
        private String label;

        /**
         * Constructor for this enum
         *
         * @param label the label for this coordinate system in the UI
         */
        private CoordinateSystem(String label) {
            this.label = label;
        }

        /**
         * Gets the label for this coordinate system as it appears in the UI
         *
         * @return the label for this coordinate system
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    // Public Constants
    ///////////

    /**
     * The expected header text
     */
    public static final String HEADER_TEXT = "Configure Sim Options";

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public SimOptionsForm(WebDriver driver) {
        super(driver);
    }

    ///////////
    // IDs & Locators
    ///////////

    /**
     * Xpath to the Options panel
     */
    private static final String OPTIONS_PANEL_XPATH = "//div[contains(@id, 'simOptionsPanel')]";

    /**
     * By locator suitable for locating the Latitude Label. This is not the
     * element containing the actual text, but instead one that has an @for
     * reference to the correct textfield.
     */
    private static final By LATITUDE_LABEL_LOCATOR = By.xpath(".//label/span[contains(text(),'Latitude')]/..");

    /**
     * By locator suitable for locating the Longitude Label. This is not the
     * element containing the actual text, but instead one that has an @for
     * reference to the correct textfield.
     */
    private static final By LONGITUDE_LABEL_LOCATOR = By.xpath(".//label/span[contains(text(),'Longitude')]/..");

    /**
     * By locator suitable for locating the Model Type label
     */
    private static final By MODEL_TYPE_LABEL_LOCATOR = By.xpath(".//label/span[contains(text(),'Model Type:')]/..");

    /**
     * By locator suitable for locating Advanced Options button associated with
     * Model Type
     */
    private static final By ADVANCED_OPTIONS_LOCATOR = By.xpath("//span[contains(@id, 'button')][text()='Advanced Options']");

    /**
     * By locator suitable for locating the Coordinate System label
     */
    private static final By COORDINATE_SYSTEM_LABEL_LOCATOR = By.xpath(".//label/span[contains(text(),'Coordinate System:')]/..");

    /**
     * By locator suitable for locating the Currently Covered label
     */
    private static final By CURRENTLY_COVERED_LABEL_LOCATOR = By.xpath(".//label/span[contains(text(),'Currently covered:')]/..");

    /**
     * The xpath of the model type dropdown selector
     */
    private static final String MODEL_TYPE_DROPDOWN_SELECTOR_XPATH = "//span[text()='Model Type:']//ancestor::div[contains(@class, 'x-form-item')]//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * The xpath of the coordinate system dropdown selector
     */
    private static final String COORDINATE_SYSTEM_DROPDOWN_SELECTOR_XPATH = "//span[text()='Coordinate System:']//ancestor::div[contains(@class, 'x-form-item')]//div[contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * Xpath prefix to options in dropdown menu
     */
    private static final String DROPDOWN_OPTION_XPATH_PREFIX = "//li[@role='option'][text()='";

    /**
     * Xpath of the selected option in drop down
     */
    private static final String SELECTED_OPTION_XPATH_PREFIX = "//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Latitude field name as displayed in UI
     */
    private static final String LATITUDE_FIELD_DISPLAYED_NAME = "Latitude";

    /**
     * Longitude field name as displayed in UI
     */
    private static final String LONGITUDE_FIELD_DISPLAYED_NAME = "Longitude";

    /**
     * Error text displayed with icon/tooltip when invalid latitude is used
     */
    private static final String INVALID_LATITUDE_ERROR_TEXT = "Latitude must be between -90 and 90.";

    /**
     * Error text displayed with icon/tooltip when invalid longitude is used
     */
    private static final String INVALID_LONGITUDE_ERROR_TEXT = "Longitude must be between -180 and 180.";

    /**
     * Error text displayed with icon/tooltip when non-numeric latitude or
     * longitude is used
     */
    private static final String NON_NUMERIC_LAT_LONG_ERROR_TEXT = "not a valid number";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the Options panel
     *
     * @return the panel element
     */
    private El getOptionsPanel() {
        return el(By.xpath(OPTIONS_PANEL_XPATH));
    }

    /**
     * Gets the label for the latitude field
     *
     * @return the latitude label
     */
    private El getLatitudeLabel() {
        return el(LATITUDE_LABEL_LOCATOR);
    }

    /**
     * Gets the label for the longitude field
     *
     * @return the longitude label
     */
    private El getLongitudeLabel() {
        return el(LONGITUDE_LABEL_LOCATOR);
    }

    /**
     * Gets the latitude field
     *
     * @return the latitude field input element
     */
    private El getLatitudeField() {
        El label = getLatitudeLabel();
        String id = label.getAttribute("for");
        return el(By.id(id));
    }

    /**
     * Gets the longitutde field
     *
     * @return the longitude field input element
     */
    private El getLongitudeField() {
        El label = getLongitudeLabel();
        String id = label.getAttribute("for");
        return el(By.id(id));
    }

    /**
     * Gets the model type label
     *
     * @return the model type label
     */
    private El getModelTypeLabel() {
        return el(MODEL_TYPE_LABEL_LOCATOR);
    }

    /**
     * Gets the model type menu
     *
     * @return the model type menu
     */
    private El getModelTypeMenu() {
        El label = getModelTypeLabel();
        String id = label.getAttribute("for");
        return el(By.id(id));
    }

    /**
     * Gets specific option from dropdown
     *
     * @param text -the text of the option to get
     * @return the option element
     */
    private El getSpecificOption(String text) {
        return el(By.xpath(DROPDOWN_OPTION_XPATH_PREFIX + text + "']"));
    }

    /**
     * Gets the Advanced Options button associated with Model Type
     *
     * @return the button element
     */
    private El getAdvancedOptionsBtn() {
        return el(ADVANCED_OPTIONS_LOCATOR);
    }

    /**
     * Gets the Coordinate system label
     *
     * @return the coordinate system label
     */
    private El getCoordinateSystemLabel() {
        return el(COORDINATE_SYSTEM_LABEL_LOCATOR);
    }

    /**
     * Gets the coordinate system menu
     *
     * @return the coordinate system menu
     */
    private El getCoordinateSystemMenu() {
        El label = getCoordinateSystemLabel();
        String id = label.getAttribute("id");
        return el(By.id(id));
    }

    /**
     * Gets the currently covered label
     *
     * @return the currently covered label
     */
    private El getCurrentlyCoveredLabel() {
        return el(CURRENTLY_COVERED_LABEL_LOCATOR);
    }

    /**
     * Gets the currently covered value element
     *
     * @return the currently covered value element
     */
    private El getCurrentlyCoveredValue() {
        El label = getCurrentlyCoveredLabel();
        String id = label.getAttribute("id");
        return el(By.id(id));
    }

    /**
     * Gets the model type dropdown selector
     *
     * @return the model type dropdown selector
     */
    private El getModelTypeSelector() {
        return el(By.xpath(MODEL_TYPE_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the coordinate system dropdown selector
     *
     * @return the coordinate system dropdown selector
     */
    private El getCoordinateSystemSelector() {
        return el(By.xpath(COORDINATE_SYSTEM_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    ///////////
    // Checkers
    ///////////

    /**
     * Checks to see if the Configure Sim Options panel is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isConfigureOptionsPanelDisplayed() {
        return isElementDisplayed(getOptionsPanel());
    }

    /**
     * Checks to see if given Fieldset is displayed
     *
     * @param fieldset -the field set expected
     * @return true if displayed, false otherwise
     */
    public boolean isFieldsetDisplayed(Fieldsets fieldset) {
        return isElementDisplayed(el(fieldset.getLocator()).webElement());
    }

    /**
     * Checks to see if Sim Center fieldset is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimCenterDisplayed() {
        return isFieldsetDisplayed(Fieldsets.SIMULATION_CENTER);
    }

    /**
     * Checks to see if Comms Model fieldset is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCommsModelDisplayed() {
        return isFieldsetDisplayed(Fieldsets.COMMUNICATIONS_MODEL);
    }

    /**
     * Checks to see if Coordinate Center fieldset is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCoordinateCenterDisplayed() {
        return isFieldsetDisplayed(Fieldsets.COORDINATE_CONVERSION);
    }

    /**
     * Checks to see if RSE Coverage fieldset is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isRseCoverageDisplayed() {
        return isFieldsetDisplayed(Fieldsets.RSE_COVERAGE);
    }

    /**
     * Checks to see if the latitude field is displayed
     *
     * @return true if the longitude field is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isLatitudeFieldDisplayed() {
        return isElementDisplayed(getLatitudeField());
    }

    /**
     * Checks to see if the longitude field is displayed in the form
     *
     * @return true if the longitude field is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isLongitudeFieldDisplayed() {
        return isElementDisplayed(getLongitudeField());
    }

    /**
     * Checks to see if the model type menu is displayed
     *
     * @return true if the model type menu is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isModelTypeMenuDisplayed() {
        return isElementDisplayed(getModelTypeMenu());
    }

    /**
     * Checks to see if the Advanced Options button is displayed
     *
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isAdvancedOptionBtnDisplayed() {
        return isElementDisplayed(getAdvancedOptionsBtn());
    }

    /**
     * Checks to see if the coordinate system menu is displayed
     *
     * @return true if the menu is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isCoordinateSystemMenuDisplayed() {
        return isElementDisplayed(getCoordinateSystemMenu());
    }

    /**
     * Checks to see if the currently covered value is displayed
     *
     * @return true if the currently covered value is displayed, false if it is
     *         not or cannot be found.
     */
    public boolean isCurrentlyCoveredValueDisplayed() {
        return isElementDisplayed(getCurrentlyCoveredValue());
    }

    /**
     * Checks if given model type is selected -- method clicks model type
     * dropdown, checks for presence of selected type, then closes dropdown (or
     * returns null and closes dropdown)
     *
     * @param type -the model type expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isModelTypeSelected(ModelType type) {
        String strType = type.getLabel();
        clickModelTypeDropdown();
        try {
            El selectedOption = getSelectedOption(strType);
            clickModelTypeDropdown();
            return selectedOption != null;
        }
        catch (NoSuchElementException e) {
            clickModelTypeDropdown();
            return false;
        }
    }

    /**
     * Checks if given coordinate system is selected -- method clicks coordinate
     * system dropdown, checks for presence of selected system, then closes
     * dropdown (or returns null and closes dropdown)
     *
     * @param coordinateSys -the coordinate system expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isCoordinateSysSelected(CoordinateSystem coordinateSys) {
        String strSys = coordinateSys.getLabel();
        clickCoordinateSystemDropdown();
        try {
            El selectedOption = getSelectedOption(strSys);
            clickCoordinateSystemDropdown();
            return selectedOption != null;
        }
        catch (NoSuchElementException e) {
            clickCoordinateSystemDropdown();
            return false;
        }
    }

    /**
     * Checks to see if Invalid Latitude error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidLatitudeErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_LATITUDE_ERROR_TEXT));
    }

    /**
     * Checks to see if Invalid Longitude error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidLongitudeErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_LONGITUDE_ERROR_TEXT));
    }

    /**
     * Checks to see if non-numeric error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isNonNumericErrorDisplayed() {
        return isNonNumericErrorDisplayed();
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Help icon for this pop-up window
     *
     * @return the newly loaded help window
     */
    public ConfigureEnvironmentHelpForm clickHelp() {
        clickSimSettingsHelpIcon();
        return getPage(ConfigureEnvironmentHelpForm.class);
    }

    /**
     * Clicks the Advanced Options button
     *
     * @return the newly loaded NS3 Advanced Options form
     */
    public EditCellularOptionsModal clickAdvancedOptions() {
        getAdvancedOptionsBtn().click();
        return getPage(EditCellularOptionsModal.class);
    }

    ///////////
    // Utilities
    ///////////

    /**
     * Checks that the expected sections are displayed in the form.
     */
    public void checkFormSections() {
        Assert.assertTrue("The Simulation Center fieldset is not displayed on the simulation options form.", isSimCenterDisplayed());
        Assert.assertTrue("The Communications Model fieldset is not displayed on the simulation options form.", isCommsModelDisplayed());
        Assert.assertTrue("The Coordinate Conversion fieldset is not displayed on the simulation options form.", isCoordinateCenterDisplayed());
        Assert.assertTrue("The RSE Coverage fieldset is not displayed on the simulation options form.", isRseCoverageDisplayed());
    }

    /**
     * Checks to see that the Latitude and Longitude fields are displayed
     */
    public void checkSimCenterFields() {
        Assert.assertTrue("The latitude field is not displayed.", isLatitudeFieldDisplayed());
        Assert.assertTrue("The longitude field is not displayed.", isLongitudeFieldDisplayed());
    }

    /**
     * Gets the text from the latitude field
     *
     * @return the current value in the latitude field
     */
    public String getLatitudeText() {
        return getLatitudeField().getAttribute("value");
    }

    /**
     * Gets the text from the longitude field
     *
     * @return the current value in the longitude field
     */
    public String getLongitudeText() {
        return getLongitudeField().getAttribute("value");
    }

    /**
     * Sets the text in the latitude field
     *
     * @param text the text to set in the latitude field
     */
    public void setLatitudeText(String text) {
        getLatitudeField().setText(text);
    }

    /**
     * Copies and pastes the text from the Simulation Center section header into
     * the Latitude text box and waits for error icon to display
     */
    public void copyPasteAlphabeticTextInLatitudeTextBox() {
        El simCenter = el(Fieldsets.SIMULATION_CENTER.getLocator());
        selectCopyAndPaste(simCenter, getLatitudeField());
        waitForElementToBeVisible(By.xpath(SPECIFIC_INVALID_NUMBER_ERROR_XPATH_PREFIX + LATITUDE_FIELD_DISPLAYED_NAME + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Copies and pastes the text from the Simulation Center section header into
     * the Longitude text box and waits for error icon to display
     */
    public void copyPasteAlphabeticTextInLongitudeTextBox() {
        El simCenter = el(Fieldsets.SIMULATION_CENTER.getLocator());
        selectCopyAndPaste(simCenter, getLongitudeField());
        waitForElementToBeVisible(By.xpath(SPECIFIC_INVALID_NUMBER_ERROR_XPATH_PREFIX + LONGITUDE_FIELD_DISPLAYED_NAME + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Sets the text of the longitude field
     *
     * @param text the text to set in the longitude field
     */
    public void setLongitudeText(String text) {
        getLongitudeField().setText(text);
    }

    /**
     * Clicks Model Type dropdown
     */
    public void clickModelTypeDropdown() {
        getModelTypeSelector().click();
    }

    /**
     * Selects the given type from model type dropdown
     *
     * @param type -the type to select
     */
    public void selectModelType(ModelType type) {
        selectFromDropdownList(getModelTypeSelector(), getSpecificOption(type.label));
    }

    /**
     * Clicks Coordinate System dropdown
     */
    public void clickCoordinateSystemDropdown() {
        getCoordinateSystemSelector().click();
    }

    /**
     * Selects the given system from coordinate system dropdown
     *
     * @param coordinateSys -the coordinate system to select
     */
    public void selectCoordinateSystem(CoordinateSystem coordinateSys) {
        selectFromDropdownList(getCoordinateSystemSelector(), getSpecificOption(coordinateSys.label));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Latitude text
     * box
     */
    public void checkLatitudeFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Latitude text box.", isFieldRequiredErrorDisplayed(LATITUDE_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Latitude text
     * box
     */
    public void checkLongitudeFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Longitude text box.", isFieldRequiredErrorDisplayed(LONGITUDE_FIELD_DISPLAYED_NAME));
    }

}
