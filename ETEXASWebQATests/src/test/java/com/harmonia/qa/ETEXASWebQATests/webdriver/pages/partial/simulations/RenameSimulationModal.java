package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Rename Simulation modal
 *
 * @author llaroussini
 */
public class RenameSimulationModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public RenameSimulationModal(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Text displayed in the rename simulation header
     */
    private static final String RENAME_SIMULATION_HEADER_TEXT = "Rename Simulation";

    /**
     * Xpath for Rename Simulation header
     */
    private static final String RENAME_SIMULATION_HEADER_XPATH = "//div[contains(@id, 'Rename')][text()='Rename Simulation']";

    /**
     * Text displayed in the rename simulation help header
     */
    private static final String RENAME_SIMULATION_HELP_HEADER_TEXT = "Rename Simulation Help";

    /**
     * Text displayed in the Help Content area
     */
    private static final String HELP_CONTENT_TEXT = "Edit the name of the selected simulation. The simulation name must be unique in the composite.";

    /**
     * Text associated with updating simulation alert
     */
    private static final String UPDATING_SIM_ALERT_TEXT = "Updating Simulation...";

    /**
     * The xpath of the name text box
     */
    private static final String NAME_TEXT_BOX_XPATH = "//input[contains(@id, 'Rename')][@name='simulationName']";

    /**
     * Name field name as displayed in UI
     */
    private static final String NAME_FIELD_DISPLAYED_NAME = "Name";

    /**
     * Error text displayed with icon/tooltip when invalid sim name is used
     */
    private static final String INVALID_SIM_NAME_ERROR_TEXT = "Simulation name must consist of only letters, numbers, dashes, and spaces";

    ///////////
    //Getters
    ///////////

    /**
     * The xpath to get from the modal header to the help icon
     *
     * @return the help icon
     */
    private El getRenameSimHelpIcon() {
        return getHelpIcon(RENAME_SIMULATION_HEADER_TEXT);
    }

    /**
     * Gets the name text box
     *
     * @return the name text box
     */
    private El getNameTextBox() {
        return el(By.xpath(NAME_TEXT_BOX_XPATH));
    }

    /**
     * Gets the rename button
     *
     * @return the rename button
     */
    private El getUpdateBtn() {
        return getFormBtn(RENAME_SIMULATION_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(RENAME_SIMULATION_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the cancel button
     *
     * @return the cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(RENAME_SIMULATION_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets By associated with 'Updating Simulation' alert
     *
     * @return by for alert window
     */
    private By renamingSimAlert() {
        return By.xpath(CONTENT_AREA_XPATH_PREFIX + UPDATING_SIM_ALERT_TEXT + "')]");
    }

    /**
     * Gets the value in the name text box
     *
     * @return the displayed Name value
     */
    public String getName() {
        return getNameTextBox().getAttribute("value");
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the rename simulation header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isRenameSimHeaderDisplayed() {
        return isHeaderDisplayed(RENAME_SIMULATION_HEADER_TEXT);
    }

    /**
     * Checks to see if the rename simulation help header is displayed
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isRenameSimHelpHeaderDisplayed() {
        return isHeaderDisplayed(RENAME_SIMULATION_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the rename simulation help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isRenameSimHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @return true if the help icon is displayed, false otherwise
     */
    public boolean isRenameSimHelpIconDisplayed() {
        return isHelpIconDisplayed(RENAME_SIMULATION_HEADER_TEXT);
    }

    /**
     * Checks to see if the name text box is displayed
     *
     * @return true if the name text box is displayed, false otherwise
     */
    public boolean isNameTextBoxDisplayed() {
        return isElementDisplayed(getNameTextBox());

    }

    /**
     * Checks to see if the update button is displayed
     *
     * @return true if the update button is displayed, false otherwise
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the reset button is displayed
     *
     * @return true if the reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());

    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());

    }

    /**
     * Checks to see if the help content is displayed
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help window
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(RENAME_SIMULATION_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if Sim Name Required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimNameRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks to see if Sim Name Whitespace error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isSimNameWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(NAME_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks to see if Invalid Sim Name error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidSimNameErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_SIM_NAME_ERROR_TEXT));
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Sets the name of the simulation and checks field to ensure text is
     * successfully set
     *
     * @param name -the name of the simulation to set
     */
    public void setName(String name) {
        getNameTextBox().setText(name);
        checkNameField(name);
    }

    /**
     * Sets the name associated with the given simulation
     *
     * @param sim -the template simulation being created
     */
    public void setName(TemplateSimulation sim) {
        String name = sim.getName();
        getNameTextBox().setText(name);
    }

    /**
     * Clicks the Update button and waits for updating sim alert to disappear
     *
     * @return the newly loaded Simulations Page
     */
    public SimulationsPage clickUpdate() {
        getUpdateBtn().click();
        waitUntilAlertIsNotPresent(renamingSimAlert(), PAGE_LOAD_TIMEOUT);
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Update button used when errors expected
     */
    public void clickUpdateNoRtrn() {
        getUpdateBtn().click();
    }

    /**
     * Click the Reset button
     */
    public void clickReset() {
        getResetBtn().click();
    }

    /**
     * Click the Cancel button
     *
     * @return the newly loaded simulations page
     */
    public SimulationsPage clickCancel() {
        getCancelBtn().click();
        return getPage(SimulationsPage.class);
    }

    /**
     * Click the Rename Simulation Help icon
     */
    public void clickRenameSimHelp() {
        getRenameSimHelpIcon().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(RENAME_SIMULATION_HELP_HEADER_TEXT);
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + RENAME_SIMULATION_HELP_HEADER_TEXT + "')]"));
    }

    /**
     * Clicks the close icon
     */
    public void clickCloseIcon() {
        clickCloseIcon(RENAME_SIMULATION_HEADER_TEXT);
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     */
    public void checkRenameSimHeaderIcons() {
        checkHeaderIcons(RENAME_SIMULATION_HEADER_TEXT);
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Rename Simulation Help header not displayed as expected.", isRenameSimHelpHeaderDisplayed());
        Assert.assertTrue("Rename Simulation Help content not displayed as expected.", isRenameSimHelpContentDisplayed());
    }

    /**
     * Checks for the presence of the Name text box
     */
    public void checkFields() {
        Assert.assertTrue("The Name text box is not displayed.", isNameTextBoxDisplayed());
    }

    /**
     * Checks for the presence of the Update, Reset, and Cancel buttons
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed.", isCancelBtnDisplayed());
    }

    /**
     * Checks that displayed name matched expected name
     *
     * @param name -the name expected
     */
    public void checkNameField(String name) {
        Assert.assertEquals("Displayed name does not match expected name.", name, getName());
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Name text box
     */
    public void checkNameFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Name text box.", isFieldRequiredErrorDisplayed(NAME_FIELD_DISPLAYED_NAME));
    }

    /**
     * Verifies leading/trailing whitespace error icon/tooltip displayed with
     * Name text box
     */
    public void checkNameWhitespaceErrorDisplayed() {
        Assert.assertTrue("Leading/trailing whitespace error not displayed as expected with Name text box.", isWhitespaceErrorDisplayed(NAME_FIELD_DISPLAYED_NAME));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(RENAME_SIMULATION_HEADER_XPATH), 20);
    }

}