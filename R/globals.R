if (FALSE) {
	options(shiny.reactlog=TRUE)
	options(warn=2)
}
if (TRUE) {
	globals <- list(
		TABLES = c(
			"Appstate", "Datasets", "Datatypes", "Mapping_Papers_Datasets", "Mapping_Papers_Models", "Methods", 
			"Models", "Papers", "Platforms", "Samples", "Settings", "mapping_groups_datasets", "mapping_groups_models", 
			"mapping_groups_resources", "mapping_users_datasets", "mapping_users_groups", "mapping_users_models", 
			"mapping_users_resources", "mapping_users_sessions", "users"
		),
		SODIUM_HASHED = FALSE,
		# TODO: migrate this info into database
		FEATURE_MAPPINGS = list(
			norm.jco.train = c(`HLA-A` = "HLA.A", `HLA-C` = "HLA.C"),
			norm.jco.test = c(`HLA-A` = "HLA.A", `HLA-C` = "HLA.C")
		),
		CATEGORICALS = c("STUDIDn", "PATSTUDID", "Individual", "PATIENTEN_ID")
	)
}
