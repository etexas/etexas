package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Options form for a Composite
 *
 * @author llaroussini
 */
public class EditOptionsForm extends CompositeSettingsModal {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public EditOptionsForm(WebDriver driver) {
        super(driver);
    }

    ///////////
    // Enumerated elements
    ///////////

    /**
     * Fields displayed in the modal
     *
     * @author llaroussini
     */
    public enum EditOptionField {
        /**
         * Latitude text box
         */
        LATITUDE("Latitude (DD)", "latitude"),
        /**
         * Longitude text box
         */
        LONGITUDE("Longitude (DD)", "longitude"),
        /**
         * Geographic Calculator dropdown
         */
        GEO_CALCULATOR("Geographic Calculator", "geographicCalculator"),
        /**
         * Communications Modal dropdown
         */
        COMM_MODEL("Communications Model", "communicationsModel");

        /**
         * The label for the field as it appears in the UI
         */
        private String label;

        /**
         * The name attribute for the field as it appears in the UI
         */
        private String name;

        /**
         * Constructor for this enum
         *
         * @param label the label for the field in the UI
         * @param name the name attribut for the field in the UI
         */
        private EditOptionField(String label, String name) {
            this.label = label;
            this.name = name;
        }

        /**
         * Gets the label for the field as it appears in the UI
         *
         * @return the label for the field
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the name attribute for the field as it appears in the UI
         *
         * @return the name attribute for the field
         */
        public String getName() {
            return this.name;
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
     * Geographic Calculator options
     *
     * @author llaroussini
     */
    public enum GeoCalculatorOption {
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
         * The label for the option as it appears in the uI
         */
        private String label;

        /**
         * Constructor for this enum
         *
         * @param label the label for the option in the UI
         */
        private GeoCalculatorOption(String label) {
            this.label = label;
        }

        /**
         * Gets the label for the option as it appears in the UI
         *
         * @return the label for the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    // IDs & Locators
    ///////////

    /**
     * The expected header text
     */
    private static final String EDIT_OPTIONS_HEADER_TEXT = "Edit Options";

    /**
     * The expected help header text
     */
    private static final String EDIT_OPTIONS_HELP_HEADER_TEXT = "Edit Options Help";

    /**
     * The expected help content text
     */
    private static final String EDIT_OPTIONS_HELP_CONTENT_TEXT = "Edit the latitude (DD), longitude (DD), geographic calculator, and communications model for the selected composite.";

    /**
     * Xpath prefix for fields displayed in modal
     */
    private static final String FIELD_XPATH_PREFIX = "//div[contains(@id, 'view')][contains(@id, 'Edit')]//input[@name='";

    /**
     * Xpath prefix for buttons displayed in modal
     */
    private static final String BTN_XPATH_PREFIX = "//div[contains(@id, 'options')][contains(@id, 'Edit')]//span[text()='";

    /**
     * Xpath suffix for buttons displayed in modal
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * The xpath of the geographic calculator dropdown selector
     */
    private static final String GEO_CALCULATOR_DROPDOWN_SELECTOR_XPATH = "//div[contains(@id, 'geographic')][contains(@id, 'calculator')][contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * By to geographic calculator dropdown options
     */
    private static final By GEO_CALUCLAR_OPTIONS_BY = By.xpath("//ul[contains(@id, 'Edit-geographic-calculator')]//li");

    /**
     * The xpath of the communications model dropdown selector
     */
    private static final String COMM_MODEL_DROPDOWN_SELECTOR_XPATH = "//div[contains(@id, 'communications')][contains(@id, 'model')][contains(@class,'x-form-arrow-trigger-default')]";

    /**
     * By to communications model dropdown options
     */
    private static final By COMM_MODEL_OPTIONS_BY = By.xpath("//ul[contains(@id, 'Edit-communications-model')]//li");

    /**
     * Xpath prefix to options in dropdown menu
     */
    private static final String DROPDOWN_OPTION_XPATH_PREFIX = "//li[@role='option'][text()='";

    /**
     * Xpath of the selected option in drop down
     */
    private static final String SELECTED_OPTION_XPATH_PREFIX = "//li[contains(@class, 'x-boundlist-selected')][text()='";

    /**
     * Error text displayed with icon/tooltip when invalid latitude is used
     */
    private static final String INVALID_LATITUDE_ERROR_TEXT = "Latitude values must be in the range of -90 to 90 DD.";

    /**
     * Error text displayed with icon/tooltip when invalid longitude is used
     */
    private static final String INVALID_LONGITUDE_ERROR_TEXT = "Longitude values must be in the range of -180 to 180 DD.";

    ///////////
    // Getters
    ///////////

    /**
     * Gets the given field
     *
     * @param field -the field to get
     * @return the field element
     */
    private El getField(EditOptionField field) {
        return el(By.xpath(FIELD_XPATH_PREFIX + field.getName() + "']"));
    }

    /**
     * Gets the given button
     *
     * @param btn -the button to get
     * @return the button element
     */
    private El getBtn(BtnNames btn) {
        return el(By.xpath(BTN_XPATH_PREFIX + btn.getLabel() + BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the Geographic Calculator selector
     *
     * @return the selector element
     */
    private El getGeoCalculatorSelector(){
        return el(By.xpath(GEO_CALCULATOR_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the Communications Model selector
     *
     * @return the selector element
     */
    private El getCommModelSelector() {
        return el(By.xpath(COMM_MODEL_DROPDOWN_SELECTOR_XPATH));
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
     * Gets the selected option within drop down
     *
     * @param option -the option selected
     * @return the selected option element
     */
    private El getSelectedOption(String option) {
        return el(By.xpath(SELECTED_OPTION_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets the value from the given field
     *
     * @param field -the field from which to get the displayed value
     * @return the displayed value in the given field as string
     */
    public String getEditOptionFieldValueStr(EditOptionField field) {
        return getField(field).getAttribute("value");
    }

    /**
     * Gets the value from the given field
     *
     * @param field -the field from which to get the displayed value
     * @return the displayed value in the given field as Double
     */
    public Double getEditOptionFieldValueDouble(EditOptionField field) {
        String value = getField(field).getAttribute("value");
        return Double.parseDouble(value);
    }

    ///////////
    // Checkers
    ///////////

    /**
     * Checks if Edit Options Header is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isEditOptionsHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_OPTIONS_HEADER_TEXT);
    }

    /**
     * Checks if Edit Options Help Header is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isEditOptionsHelpHeaderDisplayed(){
        return isHeaderDisplayed(EDIT_OPTIONS_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if given field is displayed
     *
     * @param field -the field expected
     * @return true if displayed, false otherwise
     */
    public boolean isFieldDisplayed(EditOptionField field) {
        return isElementDisplayed(getField(field));
    }

    /**
     * Checks to see if given button is displayed
     *
     * @param btn -the button expected
     * @return true if displayed, false otherwise
     */
    public boolean isBtnDisplayed(BtnNames btn) {
        return isElementDisplayed(getBtn(btn));
    }

    /**
     * Checks if given communications model is selected -- method clicks model
     * type dropdown, checks for presence of selected type, then closes dropdown
     * (or returns null and closes dropdown)
     *
     * @param model -the communication model expected to be selected
     * @return true if selected; false otherwise
     */
    public boolean isCommModelSelected(ModelType type) {
        String strType = type.getLabel();
        clickCommModelDropdown();
        try {
            El selectedOption = getSelectedOption(strType);
            clickCommModelDropdown();
            return selectedOption != null;
        }
        catch (NoSuchElementException e) {
            clickCommModelDropdown();
            return false;
        }
    }

    /**
     * Checks if given geographic calculator option is selected -- method clicks
     * geographic calculator dropdown, checks for presence of selected option,
     * then closes dropdown (or returns null and closes dropdown)
     *
     * @param geoCalcOption -the geographic calculator option expected to be
     *        selected
     * @return true if selected; false otherwise
     */
    public boolean isGeoCalculatorOptionSelected(GeoCalculatorOption geoCalcOption) {
        String option = geoCalcOption.getLabel();
        clickGeoCalculatorDropdown();
        try {
            El selectedOption = getSelectedOption(option);
            clickGeoCalculatorDropdown();
            return selectedOption != null;
        }
        catch (NoSuchElementException e) {
            clickGeoCalculatorDropdown();
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
        return isErrorToolTipDisplayed(INVALID_LATITUDE_ERROR_TEXT);
    }

    /**
     * Checks to see if Invalid Longitude error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidLongitudeErrorDisplayed() {
        return isErrorToolTipDisplayed(INVALID_LONGITUDE_ERROR_TEXT);
    }

    /**
     * Checks to see if non-numeric error icon is displayed with Latitude
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLatitudeNonNumericErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(EditOptionField.LATITUDE.label);
    }

    /**
     * Checks to see if non-numeric error icon is displayed with Longitude
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLongitudeNonNumericErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(EditOptionField.LONGITUDE.label);
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Help icon in the Edit Options header
     */
    public void clickEditOptionsHelpIcon() {
        clickHelpIcon(EDIT_OPTIONS_HEADER_TEXT);
    }

    /**
     * Clicks the Close icon in the Edit Options header
     */
    public void clickEditOptionsCloseIcon() {
        clickCloseIcon(EDIT_OPTIONS_HEADER_TEXT);
    }

    /**
     * Clicks the Help OK button
     */
    public void clickEditOptionsHelpOKBtn() {
        clickHelpOKBtn(EDIT_OPTIONS_HELP_HEADER_TEXT);
    }

    /**
     * Sets the text in the latitude field
     *
     * @param text the text to set in the latitude field
     */
    public void setLatitudeText(String text) {
        getField(EditOptionField.LATITUDE).setText(text);
    }

    /**
     * Sets the text of the longitude field
     *
     * @param text the text to set in the longitude field
     */
    public void setLongitudeText(String text) {
        getField(EditOptionField.LONGITUDE).setText(text);
    }

    /**
     * Clicks Communications Model dropdown
     */
    public void clickCommModelDropdown() {
        getCommModelSelector().click();
    }

    /**
     * Selects the given type from communications model dropdown
     *
     * @param type -the type to select
     */
    public void selectCommModel(ModelType type) {
        selectFromDropdownList(getCommModelSelector(), getSpecificOption(type.label));
        waitForElementToBeInvisible(COMM_MODEL_OPTIONS_BY);
    }

    /**
     * Clicks Geographic Calculator dropdown
     */
    public void clickGeoCalculatorDropdown() {
        getGeoCalculatorSelector().click();
    }

    /**
     * Selects the given option from Geographic Calculator dropdown
     *
     * @param geoCalcOption -the geographic calculator option to select
     */
    public void selectGeoCalculatorOption(GeoCalculatorOption geoCalcOption) {
        selectFromDropdownList(getGeoCalculatorSelector(), getSpecificOption(geoCalcOption.getLabel()));
        waitForElementToBeInvisible(GEO_CALUCLAR_OPTIONS_BY);
    }


    /**
     * Clicks the Update button and waits for modal to close
     *
     * @param success -true if success expected, false otherwise
     * @return the newly loaded Composite Options Partial Page if success,
     *         otherwise no return
     */
    public CompositeOptionsPartialPage clickUpdateBtn(boolean success) {
        getBtn(BtnNames.UPDATE).click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(GEO_CALCULATOR_DROPDOWN_SELECTOR_XPATH));
            return getPage(CompositeOptionsPartialPage.class);
        }
        else {
            return null;
        }
    }

    /**
     * Clicks the Reset button
     */
    public void clickResetBtn() {
        getBtn(BtnNames.RESET).click();
    }

    /**
     * Clicks the Cancel button
     *
     * @return the newly loaded Composite Options Partial Page
     */
    public CompositeOptionsPartialPage clickCancelBtn() {
        getBtn(BtnNames.CANCEL).click();
        return getPage(CompositeOptionsPartialPage.class);
    }

    ///////////
    // Utilities
    ///////////

    /**
     * Verifies the Edit Options header is displayed
     */
    public void checkEditOptionsHeader() {
        Assert.assertTrue("Edit Options header is not displayed as expected.", isEditOptionsHeaderDisplayed());
    }

    /**
     * Verifies the Edit Options help header is displayed
     */
    public void checkEditOptionsHelpHeader() {
        Assert.assertTrue("Edit Options help header is not displayed as expected.", isEditOptionsHelpHeaderDisplayed());
    }

    /**
     * Verifies the Edit Options help content text is displayed
     */
    public void checkEditOptionsHelpContent() {
        Assert.assertTrue("Edit Options help content text is not displayed as expected.", isContentDisplayed(EDIT_OPTIONS_HELP_CONTENT_TEXT));
    }

    /**
     * Verifies OK button is displayed in Edit Options Help modal
     */
    public void checkHelpOKBtn() {
        Assert.assertTrue("OK button not displayed as expected in Edit Options Help modal.", isHelpOKBtnDisplayed(EDIT_OPTIONS_HELP_HEADER_TEXT));
    }

    /**
     * Verifies header text, content text, and presence of OK button in Edit
     * Options Help modal
     */
    public void checkEditOptionsHelpModal() {
        checkEditOptionsHelpHeader();
        checkEditOptionsHelpContent();
        checkHelpOKBtn();
    }

    /**
     * Verifies the Help and Close icons are displayed in the Edit Options
     * header
     */
    public void checkEditOptionsHeaderIcons() {
        checkHeaderIcons(EDIT_OPTIONS_HEADER_TEXT);
    }

    /**
     * Verifies the following fields are displayed: Latitude text box, Longitude
     * text box, Geographic Calculator dropdown, and Communications Model
     * dropdown
     */
    public void checkFields() {
        Assert.assertTrue("The Latitude text box is not displayed.", isFieldDisplayed(EditOptionField.LATITUDE));
        Assert.assertTrue("The Longitude text box is not displayed.", isFieldDisplayed(EditOptionField.LONGITUDE));
        Assert.assertTrue("The Geographic Calculator dropdown is not displayed.", isFieldDisplayed(EditOptionField.GEO_CALCULATOR));
        Assert.assertTrue("The Communications Model dropdown is not displayed.", isFieldDisplayed(EditOptionField.COMM_MODEL));
    }

    /**
     * Verifies the following buttons are displayed: Update, Reset, and Cancel
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isBtnDisplayed(BtnNames.UPDATE));
        Assert.assertTrue("The Reset button is not displayed", isBtnDisplayed(BtnNames.RESET));
        Assert.assertTrue("The Cancel button is not displayed", isBtnDisplayed(BtnNames.CANCEL));
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
        Assert.assertEquals("The Latitude value is not displayed as expected.", latitude, getEditOptionFieldValueDouble(EditOptionField.LATITUDE));
        Assert.assertEquals("The Longitude value is not displayed as expected.", longitude, getEditOptionFieldValueDouble(EditOptionField.LONGITUDE));
        Assert.assertEquals("The Geographic Calculator value is not displayed as expected.", geoCalc, getEditOptionFieldValueStr(EditOptionField.GEO_CALCULATOR));
        Assert.assertEquals("The Communications Model value is not displayed as expected.", commModel, getEditOptionFieldValueStr(EditOptionField.COMM_MODEL));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Latitude text box
     */
    public void checkLatitudeFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Latitude text box.", isFieldRequiredErrorDisplayed(EditOptionField.LATITUDE.getLabel()));
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Latitude text box
     */
    public void checkLongitudeFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Longitude text box.", isFieldRequiredErrorDisplayed(EditOptionField.LONGITUDE.getLabel()));
    }

}
