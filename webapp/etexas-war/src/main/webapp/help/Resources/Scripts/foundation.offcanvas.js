/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
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
 * #L%
 */
/*
 * Foundation Responsive Library
 * http://foundation.zurb.com
 * Copyright 2014, ZURB
 * Free to use under the MIT license.
 * http://www.opensource.org/licenses/mit-license.php
*/

;(function ($, window, document, undefined) {
  'use strict';

  Foundation.libs.offcanvas = {
    name : 'offcanvas',

    version : '5.5.0',

    settings : {
      open_method: 'move',
      close_on_click: false
    },

    init : function (scope, method, options) {
      this.bindings(method, options);
    },

    events : function () {
      var self = this,
          S = self.S,
          move_class = '',
          right_postfix = '',
          left_postfix = '';

      if (this.settings.open_method === 'move') {
        move_class = 'move-';
        right_postfix = 'right';
        left_postfix = 'left';
      } else if (this.settings.open_method === 'overlap_single') {
        move_class = 'offcanvas-overlap-';
        right_postfix = 'right';
        left_postfix = 'left';
      } else if (this.settings.open_method === 'overlap') {
        move_class = 'offcanvas-overlap';
      }

      S(this.scope).off('.offcanvas')
        .on('click.fndtn.offcanvas', '.left-off-canvas-toggle', function (e) {
          self.click_toggle_class(e, move_class + right_postfix);
          if (self.settings.open_method !== 'overlap'){
            S('.left-submenu').removeClass(move_class + right_postfix);
          }
          $('.left-off-canvas-toggle').attr('aria-expanded', 'true');
          $("aside").show();
        })
        .on('click.fndtn.offcanvas', '.left-off-canvas-menu a', function (e) {
          var settings = self.get_settings(e);
          var parent = S(this).parent();

          if(settings.close_on_click && !parent.hasClass('has-submenu') && !parent.hasClass('back')){
            self.hide.call(self, move_class + right_postfix, self.get_wrapper(e));
            parent.parent().removeClass(move_class + right_postfix);
          }else if(S(this).parent().hasClass('has-submenu')){
            e.preventDefault();
            S(this).siblings('.left-submenu').toggleClass(move_class + right_postfix);
          }else if(parent.hasClass('back')){
            e.preventDefault();
            parent.parent().removeClass(move_class + right_postfix);
          }
          $('.left-off-canvas-toggle').attr('aria-expanded', 'true');
          $("aside").show();
        })
        .on('click.fndtn.offcanvas', '.right-off-canvas-toggle', function (e) {
          self.click_toggle_class(e, move_class + left_postfix);
          if (self.settings.open_method !== 'overlap'){
            S('.right-submenu').removeClass(move_class + left_postfix);
          }
          $('.right-off-canvas-toggle').attr('aria-expanded', 'true');
          $("aside").show();
        })
        .on('click.fndtn.offcanvas', '.right-off-canvas-menu a', function (e) {
          var settings = self.get_settings(e);
          var parent = S(this).parent();

          if(settings.close_on_click && !parent.hasClass('has-submenu') && !parent.hasClass('back')){
            self.hide.call(self, move_class + left_postfix, self.get_wrapper(e));
            parent.parent().removeClass(move_class + left_postfix);
          }else if(S(this).parent().hasClass('has-submenu')){
            e.preventDefault();
            S(this).siblings('.right-submenu').toggleClass(move_class + left_postfix);
          }else if(parent.hasClass('back')){
            e.preventDefault();
            parent.parent().removeClass(move_class + left_postfix);
          }
          $('.right-off-canvas-toggle').attr('aria-expanded', 'true');
          $("aside").show();
        })
        .on('click.fndtn.offcanvas', '.exit-off-canvas', function (e) {
          self.click_remove_class(e, move_class + left_postfix);
          S('.right-submenu').removeClass(move_class + left_postfix);
          if (right_postfix){
            self.click_remove_class(e, move_class + right_postfix);
            S('.left-submenu').removeClass(move_class + left_postfix);
          }
          $('.right-off-canvas-toggle').attr('aria-expanded', 'true');
          $("aside").show();
        })
        .on('click.fndtn.offcanvas', '.exit-off-canvas', function (e) {
          self.click_remove_class(e, move_class + left_postfix);
          $("aside").hide();
          $('.left-off-canvas-toggle').attr('aria-expanded', 'false');
          if (right_postfix) {
            self.click_remove_class(e, move_class + right_postfix);
            $('.right-off-canvas-toggle').attr('aria-expanded', 'false');
          }
        });
    },

    toggle: function(class_name, $off_canvas) {
      $off_canvas = $off_canvas || this.get_wrapper();
      if ($off_canvas.is('.' + class_name)) {
        this.hide(class_name, $off_canvas);
      } else {
        this.show(class_name, $off_canvas);
      }
    },

    show: function(class_name, $off_canvas) {
      $off_canvas = $off_canvas || this.get_wrapper();
      $off_canvas.trigger('open').trigger('open.fndtn.offcanvas');
      $off_canvas.addClass(class_name);
    },

    hide: function(class_name, $off_canvas) {
      $off_canvas = $off_canvas || this.get_wrapper();
      $off_canvas.trigger('close').trigger('close.fndtn.offcanvas');
      $off_canvas.removeClass(class_name);
    },

    click_toggle_class: function(e, class_name) {
      e.preventDefault();
      var $off_canvas = this.get_wrapper(e);
      this.toggle(class_name, $off_canvas);
    },

    click_remove_class: function(e, class_name) {
      e.preventDefault();
      var $off_canvas = this.get_wrapper(e);
      this.hide(class_name, $off_canvas);
    },

    get_settings: function(e) {
      var offcanvas  = this.S(e.target).closest('[' + this.attr_name() + ']');
      return offcanvas.data(this.attr_name(true) + '-init') || this.settings;
    },

    get_wrapper: function(e) {
      var $off_canvas = this.S(e ? e.target : this.scope).closest('.off-canvas-wrap');

      if ($off_canvas.length === 0) {
        $off_canvas = this.S('.off-canvas-wrap');
      }
      return $off_canvas;
    },

    reflow : function () {}
  };
}(jQuery, window, window.document));