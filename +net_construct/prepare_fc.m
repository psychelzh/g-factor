import net_construct.transform_to_fc

file_config_proc = fullfile("config.local", "process_net_construct.xlsx");
file_config_path = fullfile("config.local", "neural_raw_path.csv");
config_path_input = readtable(file_config_path, ...
    Delimiter=',', TextType='string');
config_path_output = dictionary( ...
    "yes", fullfile('data', 'neural'), ...
    "no", fullfile('data', 'neural-legacy'));
configs_proc = readtable(file_config_proc, TextType='string');
subjs_subset = readmatrix(fullfile("data", "subjs_neural"));

for row = 1:height(configs_proc)
    config_proc = configs_proc(row, :);
    fprintf("Begin new config:\n")
    disp(config_proc)
    if config_proc.status == "done"
        fprintf("Already done! Contine to next.\n")
        continue
    end
    if config_proc.use_gretna == "yes"
        config_proc.filt = config_proc.filt + "_gretna";
    end
    try
        transform_to_fc(config_path_input, config_proc, subjs_subset, ...
            config_path_output(config_proc.use_gretna))
        configs_proc.status(row) = "done";
    catch ME
        configs_proc.status(row) = "error";
    end
    writetable(configs_proc, file_config_proc)
    if exist("ME", "var")
        rethrow(ME)
    end
end
