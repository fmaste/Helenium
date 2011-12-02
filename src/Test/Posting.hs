module Test.Posting where

import Helenium

main = runTest test

test = do
	echo "Go to olx US home."
	goTo "http://www.olx.com"
	echo "Press the post an ad button."
	postingButton <- getElementByXPath 
		"/html/body/div/div[2]/div[3]/div[2]/div/div/div/a/div/div[2]/div/span"
	clickElement postingButton
	echo "Set 90210 as location."
	location <- getElementById "autocomplete"
	sendKeysToElement location "90210"
	echo "Select vehicles as category."
	category <- getElementByXPath 
		"/html/body/div[3]/div[3]/div[2]/div[2]/div/div/div/form/div[2]/p/span/select/option[9]"
	clickElement category
	echo "Select cars as subcategory."
	subcategory <- getElementByXPath 
		"/html/body/div[3]/div[3]/div[2]/div[2]/div/div/div/form/div[2]/p/span/select[2]/option[2]"
	clickElement subcategory
	echo "Set a title."
	title <- getElementById "title"
	timestamp <- getCurrentTime
	sendKeysToElement title timestamp
	echo "Set a price."
	price <- getElementById "C"
	sendKeysToElement price "1"
	echo "Set a description."
	changeFocusToIframeById "description_ifr"
	description <- getElementById "tinymce"
	timestamp' <- getCurrentTime
	sendKeysToElement description ("Description description description " ++ timestamp')
	changeFocusToDefaultIframe
	echo "Set an email."
	email <- getElementById "email"
	sendKeysToElement email "automatic@mailcatch.com"
	echo "Submit"
	submit <- getElementById "btnPublish"
	clickElement submit
	echo "Wait for the submit to be procesed."
	sleep 10
	echo "Assert the correct posting success url."
	postingSuccessUrl <- getUrl
	assertEqual postingSuccessUrl "http://www.olx.com/posting_success.php"
	congratulationElement <- getElementByXPath "/html/body/div/div[3]/div/div/p/strong"
	echo "Assert congratulation text."
	congratulationText <- getElementText congratulationElement
	assertPrefix "Congratulations! Your Ad has been Published." congratulationText
	echo "Go to the item by clicking the provided link."
	itemLink <- getElementByXPath "/html/body/div/div[3]/div/p[2]/a"
	clickElement itemLink
	echo "Assert the item title."
	itemTitle <- getElementById "olx_item_title"
	itemTitleText <- getElementText itemTitle
	assertInfix timestamp itemTitleText

