classdef TEC_FILE_LOG < ORDERED_TEC.TEC_FILE_BASE
    %UNTITLED8 Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        Time_Begin;
        Time_End;
        UsingTime;
        Size;
        Echo_Text;
        Json_Text;
        Xml_Text;
        Zones;
    end
    
    methods
        function obj = TEC_FILE_LOG(varargin)
            if nargin==0
            elseif nargin==1
                if isa(varargin{1},'ORDERED_TEC.TEC_FILE')
                    if isscalar(varargin{1})
                        tec_file = varargin{1};
                        obj.FilePath = tec_file.FilePath;
                        obj.FileName = tec_file.FileName;
                        obj.Title = tec_file.Title;
                        obj.Variables = tec_file.Variables;
                        obj.FileType = tec_file.FileType;
                        obj.Auxiliary = tec_file.Auxiliary;
                    else
                        tec_file_m = varargin{1};
                        obj = repmat(ORDERED_TEC.TEC_FILE_LOG,size(tec_file_m));
                        for kk = 1:numel(tec_file_m)
                            obj(kk) = ORDERED_TEC.TEC_FILE_LOG(tec_file_m(kk));
                        end
                    end
                elseif(isa(varargin{1},'numeric') && isscalar(varargin{1}) && mod(varargin{1},1)==0)
                    obj(varargin{1}) = ORDERED_TEC.TEC_FILE_LOG;
                else
                    ME = MException('TEC_FILE_LOG:TypeWrong', 'constructor type wrong');
                    throw(ME);
                end
            else
                ME = MException('TEC_FILE_LOG:NArgInWrong', 'too many input arguments');
                throw(ME);
            end
        end
        
        function write_echo(obj,fid)
            if nargin==1
                fid = fopen(fullfile(obj.FilePath,[obj.FileName,'.txt']),'w');
                if fid==-1
                    ME = MException('TEC_FILE_LOG:FileError', 'can not open file %s.txt',obj.FileName);
                    throw(ME);
                end
                obj.write_echo(fid);
                fclose(fid);
            elseif nargin==2
                zone_n = 0;
                for ss = obj.Echo_Text
                    if strcmp(ss{1},'#ZONE#')
                        zone_n = zone_n + 1;
                        obj.Zones(zone_n).write_echo(fid);
                    else
                        fprintf(fid,'%s\n',ss{1});
                    end
                end
            end
        end
        
        function write_json(obj,depth,fid)
            if nargin==1
                depth = 0;
                obj.write_json(depth);
            elseif nargin==2
                fid = fopen(fullfile(obj.FilePath,[obj.FileName,'.json']),'w');
                if fid==-1
                    ME = MException('TEC_FILE_LOG:FileError', 'can not open file %s.json',obj.FileName);
                    throw(ME);
                end
                obj.write_json(depth,fid);
                fclose(fid);
            elseif nargin==3
                zone_n = 0;
                for ss = obj.Json_Text
                    fprintf(fid,repmat('\t',1,depth));
                    if strcmp(ss{1},'#ZONE#')
                        zone_n = zone_n + 1;
                        obj.Zones(zone_n).write_json(depth+2,fid);
                    else
                        fprintf(fid,'%s\n',ss{1});
                    end
                end
            end
        end
        
    end
    
    methods (Hidden = true)
        function obj = gen_json(obj)
            obj.Json_Text = [];
            obj.Json_Text{end+1} = '{';
            buf = sprintf('\t"FileName" : "%s.plt" ,',obj.FileName); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"FilePath" : "%s" ,',obj.FilePath); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"Time" : "%s" ,', obj.Time_Begin); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"UsingTime" : %.5f ,', obj.UsingTime); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"Title" : "%s" ,', obj.Title); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"FileType_comment" : "0 = FULL, 1 = GRID, 2 = SOLUTION" ,'); obj.Json_Text{end+1} = buf;
            buf = sprintf('\t"FileType" : %i ,', obj.FileType); obj.Json_Text{end+1} = buf;
            
            buf = sprintf('\t"Variables" : [ '); obj.Json_Text{end+1} = buf;
            for v_n = 1:numel(obj.Variables)
                obj.Json_Text{end} = [obj.Json_Text{end},'"',obj.Variables{v_n},'"'];
                if v_n ~= numel(obj.Variables)
                    obj.Json_Text{end} = [obj.Json_Text{end},', '];
                end
            end
            obj.Json_Text{end} = [obj.Json_Text{end},' ] ,'];
            
            if ~isempty(obj.Auxiliary)
                buf  = sprintf('\t"Auxiliary" : {'); obj.Json_Text{end+1} = buf;
                for kk = 1:length(obj.Auxiliary)
                    buf = sprintf('\t\t"%s" : "%s"',obj.Auxiliary{kk}{1},obj.Auxiliary{kk}{2});
                    obj.Json_Text{end+1} = buf;
                    if kk~= length(obj.Auxiliary)
                        obj.Json_Text{end} = [obj.Json_Text{end},','];
                    end
                end
                buf  = sprintf('\t} ,'); obj.Json_Text{end+1} = buf;
            end
            
            buf = sprintf('\t"Zones" : ['); obj.Json_Text{end+1} = buf;
            for z_n = 1:numel(obj.Zones)
                obj.Zones(z_n) = obj.Zones(z_n).gen_json();
                obj.Json_Text{end+1} = '#ZONE#';
                if z_n~=numel(obj.Zones)
                    buf = sprintf('\t\t,'); obj.Json_Text{end+1} = buf;
                end
            end
            buf = sprintf('\t]'); obj.Json_Text{end+1} = buf;
            buf = sprintf('}'); obj.Json_Text{end+1} = buf;
        end
        
        function obj = gen_xml(obj)
        end
        
    end
    
end

