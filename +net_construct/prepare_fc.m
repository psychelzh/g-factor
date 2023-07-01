import net_construct.transform_to_fc

file_config = fullfile("config", "net_construction.csv");
file_config_path = fullfile("config", "path_input.csv");
config_path_input = readtable(file_config_path, ...
    Delimiter=',', TextType='string');
config_path_output = dictionary( ...
    "yes", fullfile('data', 'neural-gretna'), ...
    "no", fullfile('data', 'neural'));
configs = readtable(file_config, Delimiter=',', TextType='string');
subjs_subset = readmatrix(fullfile("data", "subjs_neural"));

for row = 1:height(configs)
    config = configs(row, :);
    fprintf("Begin cond: %s, use_gretna: %s, filt: %s, parcel: %s, gsr: %s.\n", ...
        config.cond, config.use_gretna, config.filt, config.parcel, config.gsr)
    if config.status == "done"
        fprintf("Already done! Contine to next.\n")
        continue
    end
    if config.use_gretna == "yes"
        config.filt = config.filt + "_gretna";
    end
    try
        transform_to_fc(config_path_input, config, subjs_subset, ...
            config_path_output(config.use_gretna))
        configs.status(row) = "done";
    catch ME
        configs.status(row) = "error";
    end
    writetable(configs, file_config)
    if exist("ME", "var")
        rethrow(ME)
    end
end
