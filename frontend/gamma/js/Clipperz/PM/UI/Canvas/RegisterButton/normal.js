/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

//
//	normal.js
//	normal
//
//	Created by Giulio Cesare Solaroli on 3/24/10
//	Copyright 2010 Clipperz
//	This code was generated by Opacity. You may use or modify it in any way.
//

var kClipperz_PM_UI_Canvas_RegisterButton_normalWidth = 282.0;
var kClipperz_PM_UI_Canvas_RegisterButton_normalHeight = 93.0;

function Clipperz_PM_UI_Canvas_RegisterButton_normal(canvas, aBackgroundColor, aDarkBackgroundColor, aLightColor, aDarkColor, aStarColor)
{
	var context = canvas.getContext("2d");
	var alignStroke;
	var resolution;
	var path;
	var pointX;
	var pointY;
	var controlPoint1X;
	var controlPoint1Y;
	var controlPoint2X;
	var controlPoint2Y;
	var gradient;
	var color;
	if (window.devicePixelRatio)
		resolution = window.devicePixelRatio;
	else
		resolution = 1.0;
	resolution *= 0.5 * (canvas.width / kClipperz_PM_UI_Canvas_RegisterButton_normalWidth + canvas.height / kClipperz_PM_UI_Canvas_RegisterButton_normalHeight);
	
	context.save();
	context.scale(canvas.width / kClipperz_PM_UI_Canvas_RegisterButton_normalWidth, canvas.height / kClipperz_PM_UI_Canvas_RegisterButton_normalHeight);
	context.clearRect(0.0, 0.0, kClipperz_PM_UI_Canvas_RegisterButton_normalWidth, kClipperz_PM_UI_Canvas_RegisterButton_normalHeight);
	
	// background
	
	alignStroke = 0.0;
	context.beginPath();
	pointX = 241.0;
	pointY = 80.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	pointX = 274.0;
	pointY = 47.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 259.103;
	controlPoint1Y = 80.0;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 274.0;
	controlPoint2Y = 65.103;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 274.0;
	pointY = 45.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 241.0;
	pointY = 12.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 274.0;
	controlPoint1Y = 26.897;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 259.103;
	controlPoint2Y = 12.0;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 60.0;
	pointY = 12.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 27.0;
	pointY = 45.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 41.897;
	controlPoint1Y = 12.0;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 27.0;
	controlPoint2Y = 26.897;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 27.0;
	pointY = 47.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 60.0;
	pointY = 80.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 27.0;
	controlPoint1Y = 65.103;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 41.897;
	controlPoint2Y = 80.0;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 241.0;
	pointY = 80.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	context.closePath();
	gradient = context.createLinearGradient(150.5, 12.0, 150.5, 80.0);
	gradient.addColorStop(0.0, aBackgroundColor);
	gradient.addColorStop(1.0, aDarkBackgroundColor);
	context.fillStyle = gradient;
	context.fill();
	
	// round
	
	alignStroke = 0.0;
	context.beginPath();
	pointX = 44.103;
	pointY = 4.014;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	pointX = 65.629;
	pointY = 10.515;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 51.706;
	controlPoint1Y = 4.217;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 59.185;
	controlPoint2Y = 6.475;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 78.65;
	pointY = 70.918;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 43.0;
	pointY = 90.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 70.676;
	controlPoint1Y = 82.788;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 57.23;
	controlPoint2Y = 89.817;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 0.056;
	pointY = 44.801;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 18.834;
	controlPoint1Y = 90.07;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = -1.18;
	controlPoint2Y = 68.879;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 44.103;
	pointY = 4.014;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 1.242;
	controlPoint1Y = 21.708;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 21.202;
	controlPoint2Y = 3.72;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 44.103;
	pointY = 4.014;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	context.closePath();
	gradient = context.createLinearGradient(39.326, 90, 39.326, 4.011);
	gradient.addColorStop(0.0, aDarkColor);
	gradient.addColorStop(1.0, aLightColor);
	context.fillStyle = gradient;
	context.fill();
	
	// *
	
	alignStroke = 0.0;
	context.beginPath();
	pointX = 23.983;
	pointY = 35.944;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	pointX = 37.25;
	pointY = 40.261;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 37.25;
	pointY = 24.963;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 48.231;
	pointY = 24.963;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 48.231;
	pointY = 40.261;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 61.498;
	pointY = 35.944;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 64.481;
	pointY = 45.402;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 50.961;
	pointY = 49.592;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 59.784;
	pointY = 62.224;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 51.659;
	pointY = 68.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 42.836;
	pointY = 56.066;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 33.759;
	pointY = 68.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 25.634;
	pointY = 62.224;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 34.521;
	pointY = 49.592;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 21.0;
	pointY = 45.402;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 23.983;
	pointY = 35.944;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	context.closePath();
	pointX = 68.607;
	pointY = 119.099;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	context.fillStyle = aStarColor;
	context.fill();
	
	alignStroke = 0.0;
	context.beginPath();
	pointX = 212.0;
	pointY = 125.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	pointX = 213.0;
	pointY = 125.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 212.5;
	pointY = 124.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	pointX = 212.0;
	pointY = 125.0;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	context.closePath();
	gradient = context.createLinearGradient(212.5, 125.0, 212.5, 124.0);
	color = "#FFFFFF";
	gradient.addColorStop(0.0, color);
	color = "#A9A9A9";
	gradient.addColorStop(1.0, color);
	context.fillStyle = gradient;
	context.fill();
	
	// flip
	
	// Setup for Shadow Effect
	color = "rgba(0.0%, 0.0%, 0.0%, 0.5)";
	context.save();
	context.shadowColor = color;
	context.shadowBlur = 0.0;
	context.shadowOffsetX = 2.0 * Math.cos(8.377) * resolution;
	context.shadowOffsetY = 2.0 * Math.sin(8.377) * resolution;
	
	// round
	
	alignStroke = 0.0;
	context.beginPath();
	pointX = 78.506;
	pointY = 70.251;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.moveTo(pointX, pointY);
	pointX = 66.155;
	pointY = 12.954;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	controlPoint1X = 59.899;
	controlPoint1Y = 57.427;
	controlPoint1X = (Math.round(resolution * controlPoint1X + alignStroke) - alignStroke) / resolution;
	controlPoint1Y = (Math.round(resolution * controlPoint1Y + alignStroke) - alignStroke) / resolution;
	controlPoint2X = 54.678;
	controlPoint2Y = 32.277;
	controlPoint2X = (Math.round(resolution * controlPoint2X + alignStroke) - alignStroke) / resolution;
	controlPoint2Y = (Math.round(resolution * controlPoint2Y + alignStroke) - alignStroke) / resolution;
	context.bezierCurveTo(controlPoint1X, controlPoint1Y, controlPoint2X, controlPoint2Y, pointX, pointY);
	pointX = 78.506;
	pointY = 70.251;
	pointX = (Math.round(resolution * pointX + alignStroke) - alignStroke) / resolution;
	pointY = (Math.round(resolution * pointY + alignStroke) - alignStroke) / resolution;
	context.lineTo(pointX, pointY);
	context.closePath();
	gradient = context.createLinearGradient(69.301, 70.251, 69.301, 12.954);
	gradient.addColorStop(0.0, aDarkColor);
	gradient.addColorStop(1.0, aLightColor);
	context.fillStyle = gradient;
	context.fill();
	
	// Shadow Effect
	context.restore();
	
	context.restore();
}
