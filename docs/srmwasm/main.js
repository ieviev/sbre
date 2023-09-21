import { dotnet } from './dotnet.js'

// Get exported methods from the .NET assembly
const { getAssemblyExports, getConfig } = await dotnet
    .withDiagnosticTracing(false)
    .create();

const config = getConfig();
const exports = await getAssemblyExports(config.mainAssemblyName);

// Expose the exports to the browser's JavaScript environment
window.srmwasm = exports;
