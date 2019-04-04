<%--
  #%L
  eTEXAS
  %%
  Copyright (C) 2016 Harmonia Holdings Group, LLC
  %%
  All rights reserved.
  -
  SBIR DATA RIGHTS
  Harmonia Holdings Group, LLC
  2020 Kraft Drive Suite 2400
  Blacksburg, VA 24060
  Contract No: DTRT57-16-c-10008
  Start Date: 01/05/2016
  End Date: 01/05/2018
  Expiration of SBIR Data Rights Period: 01/05/2022
  -
  The Government's rights to use, modify, reproduce, release, perform,
  display, or disclose technical data or computer software marked with
  this legend are restricted during the period shown as provided in
  paragraph (b)(4) of the Rights in Noncommercial Technical Data and
  Computer Software-Small Business Innovation Research (SBIR) Program
  clause contained in the above identified contract. No restrictions
  apply after the expiration date shown above. Any reproduction of
  technical data, computer software, or portions thereof marked with
  this legend must also reproduce the markings.
  -
  Contributors:
  Harmonia Holdings Group LLC: Initial API and implementation.
  #L%
  --%>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="X-UA-Compatible" content="IE=edge" />
<meta http-equiv="Cache-Control" content="private no-cache, no-store, max-age=2" />
<meta http-equiv="Pragma" content="no-cache" />
<meta http-equiv="Expires" content="2" />
<title>eTEXAS</title>

<link rel="stylesheet" type="text/css" href="/webjars/extjs/6.2.0/build/classic/theme-neptune/resources/theme-neptune-all.css" />
<link rel="stylesheet" type="text/css" href="/resources/css/default/app.css">

<script type="text/javascript" src="/webjars/extjs/6.2.0/build/ext-all.js"></script>
<script type="text/javascript" src="/webjars/extjs/6.2.0/build/classic/theme-neptune/theme-neptune.js"></script>
<script type="text/javascript" src="/webjars/three/0.86.0/build/three.js"></script>
<script type="text/javascript" src="/resources/display/controls/EtexasControls.js"></script>
<script type="text/javascript" src="/resources/display/loaders/ColladaLoader.js"></script>
<script type="text/javascript" src="/resources/display/loaders/OBJLoader.js"></script>
<script type="text/javascript" src="/resources/display/loaders/MTLLoader.js"></script>
<script type="text/javascript" src="/resources/display/Utils.js"></script>
<script type="text/javascript" src="/resources/display/MessageManager.js"></script>
<script type="text/javascript" src="/resources/display/FloatingIds.js"></script>
<script type="text/javascript" src="/resources/display/Map.js"></script>
<script type="text/javascript" src="/resources/display/HudDisplay.js"></script>
<script type="text/javascript" src="/resources/display/Models.js"></script>
<script type="text/javascript" src="/resources/display/TopographyFeature.js"></script>
<script type="text/javascript" src="/resources/display/Detector.js"></script>
<script type="text/javascript" src="/resources/display/StandaloneDevice.js"></script>
<script type="text/javascript" src="/resources/display/CellTower.js"></script>
<script type="text/javascript" src="/resources/display/Signal.js"></script>
<script type="text/javascript" src="/resources/display/Lane.js"></script>
<script type="text/javascript" src="/resources/display/Vehicle.js"></script>
<script type="text/javascript" src="/resources/display/Textures.js"></script>
<script type="text/javascript" src="/resources/display/ThreeMain.js"></script>

<script>
    Ext.Loader.setPath('ETexas', '/app');
    Ext.require('ETexas.util.Config');
    Ext.require('ETexas.util.Functions');
    Ext.require('ETexas.util.UrlProvider');
    Ext.require('ETexas.util.Validation');
    Ext.require('ETexas.view.login.LoginContainer');
    Ext.require('ETexas.view.main.Main');

    Ext.onReady(function() {

        Ext.Loader.loadScript('/overrides/Overrides.js');

        Ext.application({
            name : 'ETexas',
            appFolder : '/app',
            launch : function() {

                var user = sessionStorage.user;
                var token = sessionStorage.token;

                if (user && token) {

                    ETexas.util.Config.setUser(user);
                    ETexas.util.Config.setToken(token);
                    Ext.create('ETexas.view.main.Main');
                }
                else {

                    Ext.create('ETexas.view.login.LoginContainer');
                }
            }
        });
    });
</script>
</head>
<body>
</body>
</html>