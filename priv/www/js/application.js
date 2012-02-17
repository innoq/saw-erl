(function($){
	"use strict";

	$(function(){

		function readCanvas() {
			var scale = 10;
			var canvas = document.getElementById("canvas");
			var context = canvas.getContext('2d');
			var canvas2 = document.getElementById("canvas2");
			var context2 = canvas2.getContext('2d');
			canvas2.width = canvas.width * scale;
			canvas2.height = canvas.height * scale;
			var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
			var res = [];
			for (var col = 0; col < imageData.width; col++) {
				var colByte = 0;
				for(var row = 0; row < imageData.height; row++) {
					var index = (row * imageData.width + col) * 4 + 0;
					if (imageData.data[index] >= 255) {
						colByte = colByte | (1 << row);

						context2.beginPath();
						context2.fillStyle = "#FF0000";
						context2.arc(col * scale + (scale/2), row * scale + (scale/2), scale/2, 0, 2 * Math.PI, false);
						context2.fill();
					}
				}
				res.push(colByte);
			}
			return res;
		}

		function renderText(text, font, baseline) {
			var canvas = document.getElementById("canvas");
			var context = canvas.getContext('2d');
			context.font = font;
			canvas.width = context.measureText($("#text").val()).width;
			canvas.height = 8;
			context.fillStyle = '#f00';
			context.font = font;
			if (context.mozImageSmoothingEnabled) {
				context.mozImageSmoothingEnabled = false;
			}
			context.textBaseline = baseline;
			var y = 8;
			if (baseline == "top") {y = 0};
			if (baseline == "middle") {y = 4};
			context.fillText(text, 0, y);
			readCanvas();
		}

		$("#text, #font").keyup(function(){
			renderText($("#text").val(), $("#font").val(), $("#baseline").val());
		});

		$("#baseline").change(function(){
			renderText($("#text").val(), $("#font").val(), $("#baseline").val());
		});

		$("form").submit(function(e){
			e.preventDefault();
			$.post("http://localhost:8080/content", readCanvas().join(","))
			return false;
		});

		renderText($("#text").val(), $("#font").val(), $("#baseline").val());

	});

	}(jQuery))