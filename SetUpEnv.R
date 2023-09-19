

SetUpSaga <- function(dirWork = tempdir(), n_version = 6)
	{
	

	dirSAGAversion6 = "C:/Program\ Files\ (x86)/SAGA-GIS/"
	dirSAGAversion7 = "C:/Program\ Files/saga-7.7.0_x64/"

	dirSAGA <- dirSAGAversion6
	
	if(n_version == 7) dirSAGA <- dirSAGAversion7
	
	#list.files(dirSAGA)	

	SAGA.env <- rsaga.env(workspace = dirWork, cmd = 'saga_cmd.exe', path = dirSAGA)
	assign('SAGA.env', SAGA.env, env = .GlobalEnv)
	
	return(SAGA.env)
	}


#library(saga)
#SetUpSaga(dirdmp)
