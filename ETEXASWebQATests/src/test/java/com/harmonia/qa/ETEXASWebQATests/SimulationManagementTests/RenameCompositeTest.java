package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.RenameCompositeModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Executes steps of Rename a Composite Simulation test case, TC-083/ITC-077
 *
 * @author llaroussini
 */
public class RenameCompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used in the internal test case
     */
    private TemplateSimulation itcSimulation;

    /**
     * Composite object used in the internal test case
     */
    private CompositeSimulation itcComposite;

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user, and simulation/composite
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //Get a random simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //Get simulation/composite for itc
        itcSimulation = SimulationFactory.getTemplateSimulation(testUser, true); //Get a random simulation
        itcComposite = itcSimulation.getComposite();
        ETexasEntityManager.addEntities(itcSimulation, itcComposite);

        //Register user and create simulations/composites
        LandingPage landingPage = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);
        ETexasSimulationUtils.createTemplateSimulation(itcSimulation);

        //Log in user
        landingPage.loginAs(testUser);
    }

    /**
     * Test steps for TC-083
     */
    @Test
    public void renameCompositeExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the following options are displayed: Copy, Export, Rename, Composite Settings, Simulation Settings, Reporting, and Simulation Source.
        simPage.checkEditOptions();

        //Verify the following options are enabled: Copy, Export, Rename, Composite Settings, and Reporting; and the following options are disabled: Simulation Settings and Simulation Source.
        simPage.checkEditOptionsEnabledWithCompositeSelected();

        //Click the Rename option and verify that a Rename Composite modal is displayed.
        simPage.clickRename();
        RenameCompositeModal renameModal = getPage(RenameCompositeModal.class);

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        renameModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        renameModal.clickHelpIcon();

        //Verify that a Rename Composite Help modal is displayed with instructions for renaming a composite.
        renameModal.checkHelpModal();

        //Verify that an OK button is displayed at the bottom of the modal.
        Assert.assertTrue("OK button could not be found in Rename Composite Help modal.", renameModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        renameModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Rename Composite Help modal is still displayed after clicking OK.", renameModal.isRenameCompositeHelpHeaderDisplayed());

        //Verify that a Name text box is displayed in the Rename Composite modal.
        renameModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Rename Composite modal: Update, Reset, and Cancel.
        renameModal.checkBtns();

        //Enter a new name in the Name text box.
        String newName = RandomStringGenerator.nextLetterString(10);
        renameModal.setName(newName);

        //Click the Reset button.
        renameModal.clickReset();

        //Verify the Name text box is reset to the original value.
        renameModal.checkNameField(composite);

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the Cancel button.
        renameModal.clickCancel();

        //Verify that the Rename Composite modal is no longer displayed.
        Assert.assertFalse("The Rename Composite modal is still displayed after clicking Cancel", renameModal.isRenameCompositeHeaderDisplayed());

        //Verify that the composite name is not changed in the simulation list.
        Assert.assertTrue("The original composite could not be found after cancelling renaming.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("The renamed composited is displayed despite cancelling renaming.", simPage.isCompositeNameDisplayed(newName));

        //Select an existing composite with no executions.
        if (simPage.isCompositeSelected(composite) == false) {
            simPage.selectComposite(composite, true);
        }

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Rename option.
        simPage.clickRename();

        //Verify that the Rename Composite modal is displayed.
        Assert.assertTrue("Rename Composite modal is not displayed after selecting a composite and clicking Rename.", renameModal.isRenameCompositeHeaderDisplayed());

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the ‘x’ icon.
        renameModal.clickCloseIcon();

        //Verify that the Rename Composite modal is no longer displayed.
        Assert.assertFalse("The Rename Composite modal is still displayed after clicking the 'x' icon.", renameModal.isRenameCompositeHeaderDisplayed());

        //Verify that the composite name is not changed in the simulation list.
        Assert.assertTrue("The original composite could not be found after closing the Rename Composite modal without updating.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("The renamed composited is displayed despite closing the Rename Composite modal without updating.", simPage.isCompositeNameDisplayed(newName));

        //Select an existing composite with no executions.
        if (simPage.isCompositeSelected(composite) == false) {
            simPage.selectComposite(composite, true);
        }

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Rename option.
        simPage.clickRename();

        //Verify that the Rename Composite modal is displayed.
        Assert.assertTrue("Rename Composite modal is not displayed after selecting a composite and clicking Rename.", renameModal.isRenameCompositeHeaderDisplayed());

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the Update button.
        renameModal.clickUpdate(true);

        //Verify the Rename Composite modal is no longer displayed.
        Assert.assertFalse("The Rename Composite modal is still displayed after clicking Update.", renameModal.isRenameCompositeHeaderDisplayed());

        //Verify the updated name is displayed with the previously selected composite in the list of simulations.
        Assert.assertFalse("The original composite is still displayed after successfully renaming.", simPage.isCompositeDisplayed(composite));
        Assert.assertTrue("The renamed composited is not displayed despite successfully renaming.", simPage.isCompositeNameDisplayed(newName));

        //Update entity manager
        ETexasEntityManager.getComposite(composite.getName()).setName(newName);

        //Logout
        simPage.logout(testUser);
    }

    /**
     * Test steps for TC-077
     */
    @Test
    public void renameCompositeInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite with no executions.
        simPage.selectComposite(itcComposite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Rename option.
        simPage.clickRename();
        RenameCompositeModal renameModal = getPage(RenameCompositeModal.class);

        //Delete the text in the Name textbox.
        renameModal.setName("");

        //Click the Update button.
        renameModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating a valid composite name is required.
        Assert.assertTrue("Name required error is not displayed as expected when Name text box is blank.", renameModal.isNameRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box.
        renameModal.setName("   ");

        //Verify an error is displayed associated with the Name text box indicating a valid composite name is required.
        Assert.assertTrue("Name required error is not displayed as expected when Name text box contains whitespace only.", renameModal.isNameRequiredErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        renameModal.setName(RandomStringGenerator.nextLetterString(5) + "*$(#*$)@");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when special characters are used for composite name.", renameModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        renameModal.setName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when consecutive spaces are used in composite name.", renameModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        renameModal.setName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed with Composite Name text box when a leading space is used.",
                renameModal.isLeadingTrailingWhitespaceCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        renameModal.setName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/trailing whitespace error is not displayed with Composite Name text box when a trailing space is used.",
                renameModal.isLeadingTrailingWhitespaceCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        renameModal.setName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when a leading dash ('-') is used in composite name.", renameModal.isInvalidCompositeNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        renameModal.setName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error is not displayed as expected when a trailing dash ('-') is used in composite name.", renameModal.isInvalidCompositeNameErrorDisplayed());

        //Enter the name of an existing composite in the Name text box.
        renameModal.setName(composite);

        //Verify an error is displayed associated with the Name text box indicating the field requires a unique value.
        Assert.assertTrue("Duplicate Composite Name error not displayed as expected.", renameModal.isDuplicateCompositeNameErrorDisplayed(composite));

        //Enter a valid name in the Name text box.
        String newName = RandomStringGenerator.nextLetterString(10);
        renameModal.setName(newName);

        //Click the Update button.
        renameModal.clickUpdate(true);

        //Verify the new name is displayed with the associated simulation in the simulations list.
        Assert.assertTrue("Composite is not displayed as expected after successfully renaming.", simPage.isCompositeNameDisplayed(newName));

        //Log Out.
        simPage.logout(testUser);
    }
}
