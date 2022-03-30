// Inspired by function `js_cookie_to_r_code` from
// https://github.com/PaulC91/shinyauthr/blob/master/R/internal.R

// New Implementation by toscm (Tobias Schmidt)

// Original Implementation by PaulC91
shinyjs.getcookie = function (params) {
	var cookie = Cookies.get('shinyauthr')
	if (typeof cookie !== 'undefined') {
		Shiny.setInputValue('login_jscookie', cookie)
	} else {
		var cookie = ''
		Shiny.setInputValue('login_jscookie', cookie)
	}
}

shinyjs.setcookie = function (params) {
	Cookies.set('shinyauthr', escape(params), { expires: 7 })
	Shiny.setInputValue('login_jscookie', params)
}

shinyjs.rmcookie = function (params) {
	Cookies.remove('shinyauthr')
	Shiny.setInputValue('login_jscookie', '')
}
