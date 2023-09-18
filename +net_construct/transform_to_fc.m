function transform_to_fc(config_path_input, config_proc, subjs_subset, path_out)
% config_path_input: path for each condition, required fields: path, cond
% config_proc: process status, required fields: cond, parcel, filt, gsr
% subset: subset of subjects
% path_out: output path

match_duration = false;
% the minimal time points is 190 across conditions
truncate_points = 190;
cond = config_proc.cond;
if endsWith(cond, "eq")
    cond = erase(cond, "eq");
    match_duration = true;
end
if cond ~= "run1rest"
    path = config_path_input.path(config_path_input.cond == cond);
    [subjs, files] = match_files(path, config_proc, subjs_subset);
    num_subjs = length(subjs);
    results = cell(num_subjs, 1);
    for i_subj = progress(1:num_subjs)
        tc = load(files{i_subj});
        if match_duration
            tc.time_nodes = tc.time_nodes(1:truncate_points, :);
        end
        results{i_subj} = calc_fc(tc.time_nodes);
    end
else
    conds = unique(config_path_input.cond);
    [subjs_cond, files_cond] = arrayfun( ...
        @(cond) match_files( ...
        config_path_input.path(config_path_input.cond == cond), ...
        config_proc, subjs_subset), ...
        conds, ...
        UniformOutput=false);
    subjs = intersect(subjs_cond{1}, subjs_cond{2});
    num_subjs = length(subjs);
    results = cell(num_subjs, 1);
    for i_subj = progress(1:num_subjs)
        subj_id = subjs(i_subj);
        tcs = cellfun(@(s, f) load(f{s == subj_id}).time_nodes, ...
            subjs_cond, files_cond, ...
            UniformOutput=false);
        if match_duration
            tcs = cellfun(@(tc) tc(1:truncate_points, :), tcs, ...
                "UniformOutput", false);
        end
        tc_combined = vertcat(tcs{:});
        results{i_subj} = calc_fc(tc_combined);
    end
end

% construct as table and output as arrow feather file
tbl_fc_vecs = addvars( ...
    array2table(vertcat(results{:})), subjs', ...
    NewVariableNames="sub_id", Before=1);
out_name = fullfile(path_out, ...
    sprintf('cond-%s_parcel-%s_gsr-%s_acq-orig_fc.arrow', ...
    config_proc.cond, config_proc.parcel, ...
    erase(config_proc.gsr, "GSR")));
if ~exist(path_out, "dir")
    mkdir(path_out)
end
featherwrite(out_name, tbl_fc_vecs)
end

function [subjs, files_matches] = match_files(path, config_proc, subset)
path_data = cellstr(fullfile(path, config_proc.filt, config_proc.parcel, config_proc.gsr));
files = cell(length(path_data), 1);
for i_path = 1:length(path_data)
    files_info = dir(path_data{i_path});
    files{i_path} = fullfile({files_info.folder}, {files_info.name});
end
files = horzcat(files{:});
files_matches_existed = files(contains(files, regexpPattern("sub\d+.mat"), "IgnoreCase", true));

% subjects matching and subsetting (do not check if existed)
subjs_existed = str2double(extract(files_matches_existed, regexpPattern("(?<=sub)\d+", IgnoreCase=true)));
idx_subset = ismember(subjs_existed, subset);
files_matches = files_matches_existed(idx_subset);
subjs = subjs_existed(idx_subset);
end

function fc = calc_fc(tc)
% return a row vector of upper triangular functional connectivity

% correlation -> upper triangle -> fisher z -> normalize
cor_tc = corr(tc);
% ref: https://ww2.mathworks.cn/matlabcentral/answers/300657-after-using-triu-how-do-i-exclude-the-0s-in-the-vector#answer_232597
cor_upper_vec = cor_tc(triu(true(size(cor_tc)), 1));
fc = normalize(atanh(cor_upper_vec))';
end
