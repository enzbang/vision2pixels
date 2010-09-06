/*
 * Pixastic Lib - Histogram - v0.1.1
 * Copyright (c) 2008 Jacob Seidelin, jseidelin@nihilogic.dk, http://blog.nihilogic.dk/
 * License: [http://www.pixastic.com/lib/license.txt]
 */

Pixastic.Actions.histogram = {
	process : function(params) {

		var average = !!(params.options.average && params.options.average != "false");
		var paint = !!(params.options.paint && params.options.paint != "false");
		var color = params.options.color || "rgba(255,255,255,0.5)";
		var background = params.options.background || "";
		var colorspace = params.options.colorspace || "srgb";
		var values = [];
		if (typeof params.options.returnValue != "object") {
			params.options.returnValue = {values:[]};
		}
		var returnValue = params.options.returnValue;
		if (typeof returnValue.values != "array") {
			returnValue.values = [];
		}
		values = returnValue.values;

		if (Pixastic.Client.hasCanvasImageData()) {
			var data = Pixastic.prepareData(params);
			params.useData = false;

			for (var i=0;i<256;i++) {
				values[i] = 0;
			}

			var rect = params.options.rect;
			var p = rect.width * rect.height;

			var pix = p*4, pix1 = pix + 1, pix2 = pix + 2, pix3 = pix + 3;
			var round = Math.round;

			if (average) {
				while (p--) {
					values[ round((data[pix-=4]+data[pix+1]+data[pix+2])/3) ]++;
				}
			} else if (colorspace == "srgb") {
				while (p--) {
					values[ round(data[pix-=4]*0.2126 + data[pix+1]*0.7152 + data[pix+2]*0.0722) ]++;
				}
			} else if (colorspace == "adobergb") {
				while (p--) {
					values[ round(data[pix-=4]*0.212 + data[pix+1]*0.701 + data[pix+2]*0.087) ]++;
				}
			} else {
				while (p--) {
					values[ round(data[pix-=4]*0.3 + data[pix+1]*0.59 + data[pix+2]*0.11) ]++;
				}
			}

			if (paint) {
				var maxValue = 0;
				for (var i=0;i<256;i++) {
					if (values[i] > maxValue) {
						maxValue = values[i];
					}
				}
				var heightScale = params.height / maxValue;
				var widthScale = params.width / 256;
				var ctx = params.canvas.getContext("2d");
				if (background != "") {
					ctx.fillStyle=background;
					ctx.fillRect(0,0,params.width,params.height);
				}
				ctx.fillStyle = color;
				for (var i=0;i<256;i++) {
					ctx.fillRect(
						i * widthScale, params.height - heightScale * values[i],
						widthScale, values[i] * heightScale
					);
				}
			}

			returnValue.values = values;

			return true;
		}
	},
	checkSupport : function() {
		return Pixastic.Client.hasCanvasImageData();
	}
}
