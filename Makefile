.PHONY: all taskflow
.DELETE_ON_ERROR:

# ------------------------
# Variables

out_data = dataset_CIS/output/data.csv dataset_ESS/output/data.rds dataset_local/output/data.csv
out_models = analyses_CIS/analyses.Rout analyses_ESS/analyses.Rout analyses_local/analyses.Rout
out_desc = desc_local/desc.Rout

# ------------------------
# Main recipes

all: $(out_data) $(out_desc) $(out_models) taskflow

taskflow:
	Rscript --no-save --verbose taskflow/create_dependency_graph.R
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

desc: $(out_desc)

# ------------------------
# Datasets

dataset_CIS/output/data.csv: dataset_CIS/data.R input/encuestas_CIS.csv input/INE_census.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

dataset_ESS/output/data.rds: dataset_ESS/data.R input/ESS_combined.rds func/misc.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

dataset_local/output/data.csv: dataset_local/data.R input/INE_census.csv input/Inquisition_analysis_dataset.dta input/aggregated_TOP.csv input/elev_sd_1960_2011.csv input/distcap.csv asoc_agg/output/asoc_agg.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

asoc_agg/output/asoc_agg.csv: asoc_agg/agg.R input/asoc.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Descriptives

$(out_desc): desc_local/desc.R func/misc.R dataset_local/output/data.csv input/ESP_adm4_1960_2011.shp input/asoc.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Analyses

analyses_CIS/analyses.Rout: analyses_CIS/analyses.R func/misc.R func/sim.R dataset_CIS/output/data.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

analyses_ESS/analyses.Rout: analyses_ESS/analyses.R func/misc.R func/sim.R dataset_ESS/output/data.rds
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

analyses_local/analyses.Rout: analyses_local/analyses.R func/misc.R dataset_local/output/data.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out
