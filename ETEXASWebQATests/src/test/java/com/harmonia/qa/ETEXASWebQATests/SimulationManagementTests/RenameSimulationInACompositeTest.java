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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.RenameSimulationModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Rename a Simulation in a Composite
 * test, TC-061/ITC-042
 *
 * @author llaroussini
 */
public class RenameSimulationInACompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Original simulation object used in test case
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used in test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the internal test case
     */
    private TemplateSimulation itcTestSim;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user and simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation);

        //TODO - re-enable when ITC is updated for version 3.0
        //        //Create additional sim to be used in ITC
        //        itcTestSim = SimulationFactory.getTemplateSimulation(testUser, true); //gets random simulation
        //        ETexasEntityManager.addEntity(itcTestSim);

        //Register user
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);

        //Create simulations in UI
        ETexasSimulationUtils.createTemplateSimulation(simulation);
        //TODO - re-enable when ITC is updated for version 3.0
        //ETexasSimulationUtils.createTemplateSimulation(itcTestSim);

        //Login to eTEXAS
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-005
     */
    @Test
    public void renameSimExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify Simulation page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing Simulation with no executions.
        simPage.expandComposite(composite, true);
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the following options are displayed: Copy, Export, Rename, Composite Settings, Simulation Settings, Reporting, and Simulation Source.
        simPage.checkEditOptions();

        //Verify the following options are enabled: Copy, Export, Rename, Simulation Settings and Simulation Source and the following options are disabled: Composite Settings and Reporting.
        simPage.checkEditOptionsEnabledWithSimSelected();

        //Click the Rename button and verify that the Rename Simulation modal is displayed.
        simPage.clickRename();
        RenameSimulationModal renameModal = getPage(RenameSimulationModal.class);

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        renameModal.checkRenameSimHeaderIcons();

        //Click the ‘?’ icon.
        renameModal.clickRenameSimHelp();

        //Verify that the Rename Simulation Help modal is displayed with instructions for renaming a simulation.
        renameModal.checkHelpModal();

        //Verify that an OK button is displayed at the bottom of the modal.
        Assert.assertTrue("OK button not displayed in Rename Simulation Help modal.", renameModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        renameModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Rename Simulation Help modal is still displayed after clicking OK.", renameModal.isRenameSimHelpHeaderDisplayed());

        //Verify that a Name text box is displayed in the Rename Simulation modal.
        renameModal.checkFields();

        //Verify the following buttons are displayed at the bottom of the Rename Simulation modal: Update, Reset, and Cancel.
        renameModal.checkBtns();

        //Enter a new name in the Name text box.
        String newName = RandomStringGenerator.nextLetterString(10);
        renameModal.setName(newName);

        //Click the Reset button.
        renameModal.clickReset();

        //Verify the Name text box is reset to the original value.
        renameModal.checkNameField(simulation.getName());

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the Cancel button.
        renameModal.clickCancel();

        //Verify that the Rename Simulation modal is no longer displayed.
        Assert.assertFalse("Rename Simulation modal is still displayed after clicking Cancel.", renameModal.isRenameSimHeaderDisplayed());

        //Verify that the simulation name is not changed in the simulation list.
        Assert.assertFalse("Cancelled renamed simulation is displayed in list.", simPage.isSimDisplayed(newName));
        Assert.assertTrue("Original simulation name could not be found, despite cancelling the renaming of the simulaiton.", simPage.isSimDisplayed(simulation));

        //Select an existing simulation with no executions.
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Rename button and verify that the Rename Simulation modal is displayed.
        simPage.clickRename();

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the ‘x’ icon.
        renameModal.clickCloseIcon();

        //Verify that the Rename Simulation modal is no longer displayed.
        Assert.assertFalse("Rename Simulation modal is still displayed after clicking Close icon.", renameModal.isRenameSimHeaderDisplayed());

        //Verify that the simulation name is not changed in the simulation list.
        Assert.assertFalse("Renamed simulation is displayed in list after clicking Close icon.", simPage.isSimDisplayed(newName));
        Assert.assertTrue("Original simulation name could not be found, despite closing the Rename Simulation modal.", simPage.isSimDisplayed(simulation));

        //Select an existing simulation with no executions.
        simPage.selectSim(simulation, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Rename button and verify that the Rename Simulation modal is displayed.
        simPage.clickRename();

        //Enter a new name in the Name text box.
        renameModal.setName(newName);

        //Click the Update button.
        renameModal.clickUpdate();

        //Verify the Rename a Simulation modal is no longer displayed.
        Assert.assertFalse("Rename Simulation modal is still displayed after clicking Update.", renameModal.isRenameSimHeaderDisplayed());

        //Verify the updated name is displayed with the previously selected simulation in the list of simulations.
        Assert.assertTrue("Renamed simulation is NOT displayed in list after updating.", simPage.isSimDisplayed(newName));
        Assert.assertFalse("Original simulation still displayed, despite successfully renaming the simulation.", simPage.isSimDisplayed(simulation));

        //Verify the Type and Source columns values have not changed.
        ETexasEntityManager.getTemplateSimulation(simulation.getName()).setName(newName);
        simPage.checkSimulationDetails(simulation);

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-042
     */
    //TODO - needs to be updated based on UI changes
    //@Test
    public void renameSimInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
        //Verify Simulation page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //TODO -update for version 3.0
        //        //Select multiple existing simulations.
        //        simPage.selectAllSimulations(true);
        //
        //        //Verify the Rename button is disabled.
        //        Assert.assertFalse("The rename button is not disabled as expected when more than one simulation is selected.", simPage.isSimBtnEnabled(SimBtns.RENAME_BTN));
        //
        //        //Select a single execution
        //        simPage.selectAllSimulations(false);
        //        simPage.selectSimCheckBox(itcTestSim, true);
        //
        //        //Verify the Rename button is enabled.
        //        Assert.assertTrue("The rename button is not enabled as expected when a single simulation is selected.", simPage.isSimBtnEnabled(SimBtns.UPDATE_BTN));

        //Click the Rename button.
        simPage.clickRename();
        RenameSimulationModal renameForm = getPage(RenameSimulationModal.class);

        //With the Name text box blank, click the Update button.
        renameForm.clickUpdateNoRtrn();
        //Verify an error is displayed associated with the Name text box indicating the field is required and the acceptable values for the field.
        Assert.assertTrue("Sim Name Required error is not displayed as expected when Name field is blank.", renameForm.isSimNameRequiredErrorDisplayed());

        //Enter one or more space characters in the Name text box.
        renameForm.setName("   ");
        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Whitespace error is not displayed as expected when Name field contains whitespace only.", renameForm.isSimNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        renameForm.setName("T3$t!");
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Sim Name error is not displayed as expected when Name field contains special characters.", renameForm.isInvalidSimNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        renameForm.setName(RandomStringGenerator.nextLetterString(3) + "  " + RandomStringGenerator.nextLetterString(3));
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Sim Name error is not displayed as expected when Name field contains special characters.", renameForm.isInvalidSimNameErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        renameForm.setName(" " + RandomStringGenerator.nextLetterString(5));
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Whitespace error is not displayed as expected when Name field contains leading whitespace.", renameForm.isSimNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        renameForm.setName(RandomStringGenerator.nextLetterString(5) + " ");
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Whitespace error is not displayed as expected when Name field contains trailing whitespace.", renameForm.isSimNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        renameForm.setName("-" + RandomStringGenerator.nextLetterString(5));
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Sim Name error is not displayed as expected when Name field contains a leading dash ('-') character.", renameForm.isInvalidSimNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        renameForm.setName(RandomStringGenerator.nextLetterString(5) + "-");
        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Sim Name error is not displayed as expected when Name field contains a trailing dash ('-') character.", renameForm.isInvalidSimNameErrorDisplayed());

        //Enter a valid name in the Name text box and click the Update button.
        String newName = RandomStringGenerator.nextLetterString(5);
        renameForm.setName(newName);
        renameForm.clickUpdate();

        //Verify the new name is displayed with the associated simulation in the simulations list.
        Assert.assertTrue("New renamed simulation is not displayed in list.", simPage.isSimDisplayed(newName));
        Assert.assertFalse("Original simulation name is still displayed in list.", simPage.isSimDisplayed(itcTestSim));
        ETexasEntityManager.getTemplateSimulation(itcTestSim.getName()).setName(newName);

        //Log out
        simPage.logout(testUser);
    }
}
