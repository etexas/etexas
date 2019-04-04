package com.harmonia.qa.ETEXASWebQATests.utilities.simulations;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.VehicleInjectionCommand;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;

/**
 * Utilities pertaining to Executions
 *
 * @author llaroussini
 */
public class ETexasExecutionUtils extends ETexasCommonUtils {

    /**
     * Maximum allowable value for a random seed
     */
    public static final Integer MAX_RANDOM_SEED_VALUE = 2147483647;

    /**
     * Creates a new execution associated with the given simulation in the UI.
     * Also handles verifying the execution is created/displayed in UI. Assumes
     * the user is not logged in; handles navigation to the landing page. Logs
     * the user out at completion
     *
     * @param sim the simulation where the execution should exist
     * @return the name of the execution
     */
    public static String createNewExecution(TemplateSimulation sim) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select simulation, click Execute and verify new execution is created
        //        simPage.selectSim(sim, true);
        //        simPage.clickExecuteAndVerifyExecutions();
        //        String name = simPage.getNewExecutionName();
        //
        //        //Logout and return execution name
        //        simPage.logout(user);
        //        return name;
        return "";
    }

    /**
     * Creates a new execution associated with the given simulation in the UI.
     * Also handles verifying the execution is created/displayed in UI. Assumes
     * the user is not logged in; handles navigation to the landing page. Logs
     * the user out at completion
     *
     * @param sim the simulation where the execution should exist
     * @param rndmSeed random seed value of execution
     * @return the name of the execution
     */
    //TODO - update for version 3.0
    //	public static String createNewExecutionWithKnownRandomSeed(TemplateSimulation sim, String rndmSeed) {
    //		ETexasUser user = sim.getUser();
    //		//Login and navigate to simulations page
    //		LandingPage landing = goToLandingPage();
    //		SimulationsPage simPage = landing.loginAs(user);
    //		simPage.waitUntilLoaded();
    //		//Select simulation, click Execute and verify new execution is created
    //		simPage.selectSim(sim, true);
    //		simPage.hoverExecuteBtn();
    //		RandomSeedForm seedForm = simPage.clickEnterRandomSeed();
    //		seedForm.setRandomSeed(rndmSeed);
    //		seedForm.clickCreateAndWait();
    //		String name = simPage.getNewExecutionName();
    //		simPage.logout(user);
    //		return name;
    //	}

    /**
     * Creates a new execution in the UI, navigates to Execution Page and
     * progresses 50 steps into the execution and verifies vehicle rows display.
     * Returns to the simualtions page to verify execution displays with In
     * Progress status Then logs the user out.
     *
     * @param sim -the simulation where the executions should exist
     * @return the name of the execution
     */
    public static String createInProgressExecution(Simulation sim) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select simulation, click Execute and verify new execution is created
        //        simPage.selectSim(sim, true);
        //        simPage.clickExecuteAndVerifyExecutions();
        //        String name = simPage.getNewExecutionName();
        //
        //        //Select execution, navigate to execution details, advance 50 steps
        //        simPage.selectNewestExecution(true);
        //        ExecutionsPage executionPage = simPage.clickControlExecution();
        //        executionPage.enterSteps("50");
        //        executionPage.clickNextStep();
        //
        //        //Verify vehicle rows display
        //        VehiclesPartialPage vehicles = executionPage.clickVehiclesTab();
        //        Assert.assertTrue("Vehicle rows do not display after 50 steps into execution.", vehicles.areVehicleRowsDisplayed());
        //
        //        //Return to Simulations page and verify in progress status
        //        vehicles.clickSimulations();
        //        Assert.assertTrue("Exeuction is not displayed with In Progress status.", simPage.isExecutionAndStatusDisplayed(simPage.getNewExecutionPosition(), ExecutionStatus.IN_PROGRESS));
        //
        //        //Logout
        //        simPage.logout(user);
        //        return name;
        return "";
    }

    /**
     * Creates a new execution in the UI, navigates to Execution Page and
     * progresses 50 steps into the execution and verifies vehicle rows display.
     * Returns to the simualtions page to verify execution displays with In
     * Progress status Then logs the user out.
     *
     * @param sim -the simulation where the executions should exist
     * @param rndmSeed - the random seed value to associated with execution
     * @return the name of the execution
     */
    //TODO - update for version 3.0
    //	public static String createInProgressExecutionWithKnownRandomSeed(Simulation sim, String rndmSeed) {
    //		ETexasUser user = sim.getUser();
    //		//Login and navigate to simulations page
    //		LandingPage landing = goToLandingPage();
    //		SimulationsPage simPage = landing.loginAs(user);
    //		simPage.waitUntilLoaded();
    //		//Select simulation, click Execute and verify new execution is created
    //		simPage.selectSim(sim, true);
    //		simPage.hoverExecuteBtn();
    //		RandomSeedForm seedForm = simPage.clickEnterRandomSeed();
    //		seedForm.setRandomSeed(rndmSeed);
    //		seedForm.clickCreateAndWait();
    //		String name = simPage.getNewExecutionName();
    //		//Select execution, navigate to execution details, advance 50 steps
    //		simPage.selectNewestExecution(true);
    //		ExecutionsPage executionPage = simPage.clickControlExecution();
    //		executionPage.enterSteps("50");
    //		executionPage.clickNextStep();
    //		//Verify vehicle rows display
    //		VehiclesPartialPage vehicles = executionPage.clickVehiclesTab();
    //		Assert.assertTrue("Vehicle rows do not display after 50 steps into execution.", vehicles.areVehicleRowsDisplayed());
    //		//Return to Simulations page and verify in progress status
    //		vehicles.clickSimulations();
    //		Assert.assertTrue("Exeuction is not displayed with In Progress status.", simPage.isExecutionAndStatusDisplayed(simPage.getNewExecutionPosition(), ExecutionStatus.IN_PROGRESS));
    //		//Logout
    //		simPage.logout(user);
    //		return name;
    //	}

    /**
     * Creates a new execution in the UI, clicks Finish and verified status of
     * execution updates to Completed.
     *
     * @param sim - the simulation where the executions should exist
     * @return the name of execution
     */
    public static String createAndFinishExecution(TemplateSimulation sim) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select simulation, click Execute and verify new execution is created
        //        simPage.selectSim(sim, true);
        //        simPage.clickExecuteAndVerifyExecutions();
        //        String name = simPage.getNewExecutionName();
        //
        //        //Select execution and click Finish
        //        simPage.selectNewestExecution(true);
        //        simPage.clickFinish();
        //
        //        //Verify execution status updated to Completed
        //        Assert.assertTrue("Execution is not displayed with Completed status.", simPage.isExecutionAndStatusDisplayed(simPage.getNewExecutionPosition(), ExecutionStatus.COMPLETED));
        //
        //        //Logout
        //        simPage.logout(user);
        //        return name;
        return "";
    }

    /**
     * Creates a new execution in the UI, clicks Finish and verified status of
     * execution updates to Completed.
     *
     * @param sim - the simulation where the executions should exist
     * @param rndmSeed - the random seed value to associated with execution
     * @return the name of execution
     */
    //TODO - update for version 3.0
    //	public static String createAndFinishExecutionSetRandomSeed(TemplateSimulation sim, String rndmSeed) {
    //		ETexasUser user = sim.getUser();
    //		//Login and navigate to simulations page
    //		LandingPage landing = goToLandingPage();
    //		SimulationsPage simPage = landing.loginAs(user);
    //		simPage.waitUntilLoaded();
    //		//Select simulation, click Execute and verify new execution is created
    //		simPage.selectSim(sim, true);
    //		simPage.hoverExecuteBtn();
    //		RandomSeedForm seedForm = simPage.clickEnterRandomSeed();
    //		seedForm.setRandomSeed(rndmSeed);
    //		seedForm.clickCreateAndWait();
    //		String name = simPage.getNewExecutionName();
    //		//Select execution and click Finish
    //		simPage.selectNewestExecution(true);
    //		simPage.clickFinish();
    //		//Verify execution status updated to Completed
    //		Assert.assertTrue("Execution is not displayed with Completed status.", simPage.isExecutionAndStatusDisplayed(simPage.getNewExecutionPosition(), ExecutionStatus.COMPLETED));
    //		//Logout
    //		simPage.logout(user);
    //		return name;
    //
    //	}

    /**
     * Inserts the given inject vehicle command into the given ETexasExecution.
     * Method assumes the execution is IN PROGRESS. Method handles logout.
     *
     * @param sim -the simulation with an IN PROGRESS execution
     * @param execName -the name of execution
     * @return the sim time the command was inserted
     */
    public static String injectVehicleCommandIntoInProgressExecution(TemplateSimulation sim, String execName, VehicleInjectionCommand command) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select the simulation and execution
        //        simPage.selectSim(sim, true);
        //        simPage.selectExecution(execName, true);
        //        ExecutionsPage execPage = simPage.clickControlExecution();
        //
        //        //Step through execution by 25 steps
        //        execPage.enterSteps("50");
        //        execPage.clickNextStep();
        //        String simTime = execPage.getSimTime();
        //
        //        //Click Inject Vehicle
        //        InjectVehicleForm injectVehicle = execPage.clickInjectVehicle();
        //
        //        //Select a lane and enter a speed
        //        injectVehicle.selectLane(command);
        //        injectVehicle.setSpeed(command);
        //        injectVehicle.clickInjectVehicleOK();
        //
        //        //TODO replace with better method for waiting for an element or actio
        //        ETexasCommonUtils.sleep(500);
        //
        //        //Click Next Step
        //        execPage.clickNextStep();
        //
        //        //Navigate to Command History and ensure command displays
        //        CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
        //        commands.checkVehicleInjectionCommand(command, simTime);
        //
        //        //Logout
        //        execPage.logout(user);
        //        return simTime;
        return "";
    }

    /**
     * Inserts the given inject vehicle command into the given ETexasExecution.
     * Method assumes the execution is IN PROGRESS. Method handles finishing the
     * exectuion but does NOT LOG OUT user (due to issue noted below, will be
     * updated when resolved)
     *
     * @param sim -the simulation with an IN PROGRESS execution
     * @param execName -the name of execution
     * @return the sim time the command was inserted
     */
    public static String injectVehicleCommandIntoInProgressExecutionAndFinish(TemplateSimulation sim, String execName, VehicleInjectionCommand command) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select the simulation and execution
        //        simPage.selectSim(sim, true);
        //        simPage.selectExecution(execName, true);
        //        ExecutionsPage execPage = simPage.clickControlExecution();
        //
        //        //Step through execution by 50 steps
        //        execPage.enterSteps("50");
        //        execPage.clickNextStep();
        //        //TODO replace with more robust method for getting time
        //        String simTime = execPage.getSimTime().substring(0, 2);
        //
        //        //Click Inject Vehicle
        //        InjectVehicleForm injectVehicle = execPage.clickInjectVehicle();
        //
        //        //Select a lane and enter a speed
        //        injectVehicle.selectLane(command);
        //        injectVehicle.setSpeed(command);
        //        injectVehicle.clickInjectVehicleOK();
        //
        //        //TODO replace with better method for waiting for an element or action
        //        ETexasCommonUtils.sleep(500);
        //
        //        //Click Next Step
        //        execPage.clickNextStep();
        //
        //        //Navigate to Command History and ensure command displays
        //        CommandHistoryPartialPage commands = execPage.clickCommandHistoryTab();
        //        commands.checkVehicleInjectionCommand(command, simTime);
        //
        //        //Click Finish
        //        commands.clickFinish();
        //
        //        //execPage.logout(user);
        //        commands.clickSimulations();
        //        commnds.logout(user);
        //        return simTime;
        return "";
    }

    /**
     * Finishes an in-progress execution. Method also handles logging out.
     *
     * @param sim -the simulation with in-progress execution
     */
    public static void finishInProgressExecution(TemplateSimulation sim) {
        ETexasUser user = sim.getUser();

        //Login and navigate to simulations page
        LandingPage landing = goToLandingPage();
        SimulationsPage simPage = landing.loginAs(user);
        simPage.waitUntilLoaded();

        //TODO update when executions fully re-implemented in version 3.0
        //        //Select the simulation and execution
        //        simPage.selectSim(sim, true);
        //        simPage.selectNewestExecution(true);
        //
        //        //Click Finish
        //        simPage.clickFinish();

        //Logout
        simPage.logout(user);
    }
}
