function transform_to_fc(config_path_input, config_proc, subset, path_out)
% path: base path
% subset: subject subset
% config: required fields: cond, parcel, filt, gsr
% path_out: output path

if config_proc.cond ~= "run1rest"
    path = config_path_input.path(config_path_input.cond == config_proc.cond);
    [subjs, files] = match_files(path, config_proc, subset);
    num_subjs = length(subjs);
    results = cell(num_subjs, 1);
    for i_subj = progress(1:num_subjs)
        tc = load(files{i_subj});
        results{i_subj} = calc_fc(tc.time_nodes);
    end
else
    conds = unique(config_path_input.cond);
    [subjs, files] = arrayfun( ...
        @(cond) match_files( ...
        config_path_input.path(config_path_input.cond == cond), ...
        config_proc, subset), ...
        conds, ...
        UniformOutput=false);
    subjs_merged = intersect(subjs{1}, subjs{2});
    num_subjs = length(subjs_merged);
    results = cell(num_subjs, 1);
    for i_subj = progress(1:num_subjs)
        subj_id = subjs_merged(i_subj);
        tcs = cellfun(@(s, f) load(f{s == subj_id}).time_nodes, ...
            subjs, files, ...
            UniformOutput=false);
        tc_combined = vertcat(tcs{:});
        results{i_subj} = calc_fc(tc_combined);
    end
end

% construct as table and output as arrow feather file
tbl_fc_vecs = addvars( ...
    array2table(vertcat(results{:})), subjs', ...
    NewVariableNames="sub_id", Before=1);
out_name = fullfile(path_out, ...
    sprintf('cond-%s_parcel-%s_filt-%s_gsr-%s_fc.arrow', ...
    config_proc.cond, config_proc.parcel, extract(config_proc.filt, regexpPattern(".+pass")), erase(config_proc.gsr, "GSR")));
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
