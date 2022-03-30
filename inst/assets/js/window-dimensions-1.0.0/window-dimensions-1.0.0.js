// Based on:
// https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
var dim = [0, 0]
$(document).on('shiny:connected', function (e) {
	dim[0] = window.innerWidth
	dim[1] = window.innerHeight
	Shiny.onInputChange('dim', dim)
})
$(window).resize(function (e) {
	dim[0] = window.innerWidth
	dim[1] = window.innerHeight
	Shiny.onInputChange('dim', dim)
})
