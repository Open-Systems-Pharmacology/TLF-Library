require_relative 'scripts/utils'

task :prepare_for_build, [:product_version] do |t, args|
  product_version = sanitized_version(args.product_version)
  update_package_version(product_version)
end

def sanitized_version(version) 
  pull_request_index = version.index('-')
  return version unless pull_request_index
  version.slice(0, pull_request_index)
end

def update_package_version(version) 
  #Replace token Version: x.y.z with the version from appveyor
  replacement = {
    /Version: \d\.\d\.\d/ => "Version: #{version}"
  }

  Utils.replace_tokens(replacement, description_file)
end

def solution_dir
  File.dirname(__FILE__)
end

def description_file
  File.join(solution_dir,'DESCRIPTION')
end 